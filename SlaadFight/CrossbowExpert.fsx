// combinators
let flip f x y = f y x

let r = System.Random()
let d n dieSize x =
    [for x in 1..n -> r.Next(dieSize) + 1] |> Seq.sum |> (+) x
let badMatch sourceFile lineNumber argMatch = failwithf "%s line %s has bug: failed to match %A" sourceFile lineNumber argMatch

type RollType = Advantage | Disadvantage | Regular
type DieRoll = { N: int; DieSize: int; Plus: int } with
    static member Create(n, d) = { N = n; DieSize = d; Plus = 0 }
    static member Create(n, d, x) = { N = n; DieSize = d; Plus = x }
    static member eval (rolls: DieRoll list) =
        rolls |> Seq.sumBy (fun roll -> d roll.N roll.DieSize roll.Plus)
type DamageType = Weapon | MagicalWeapon | Poison | Fire | Lightning | Cold
type SaveAbility = Str | Dex | Con | Int | Wis | Cha
type Damage = Unavoidable of DieRoll * DamageType | SaveForHalf of SaveAbility * int * DieRoll * DamageType
type Effect = Restrained | Grappled | Prone | Afraid | Blinded | OngoingDamage of SaveAbility * int * Damage | Constrict of DieRoll
    with
    static member Exists x effects = effects |> List.contains x
    static member Add x effects = if effects |> List.contains x |> not then x :: effects else effects
    static member Remove x effects = effects |> List.filter ((<>) x)
type DirectAttack = { Text: string; ToHit: int; Damage: (DieRoll list * DamageType) list; Rider: Effect option }
type Attack = Direct of DirectAttack | Grapple | ShoveProne | BestOf of Attack * Attack with
    static member Create descriptor t d = Direct { Text = descriptor; ToHit = t; Damage = [d, Weapon]; Rider = None }
    static member CreateDualType descriptor t d1 d2 = Direct { Text = descriptor; ToHit = t; Damage = [d1;d2]; Rider = None }
    static member CreateMagic descriptor t d = Direct { Text = descriptor; ToHit = t; Damage = [d, MagicalWeapon]; Rider = None }
    static member CreateMagicWithRider descriptor t d rider = Direct { Text = descriptor; ToHit = t; Damage = [d, MagicalWeapon]; Rider = Some rider }
type AoETarget = All | EnemyOnly
type ActionEffect = Attack of Attack list | Instant of AoETarget * Damage | ConcentrationEffect of int * AoETarget * Effect list | Healing of DieRoll | BreakFree
type Action = { Name : string; Effect: ActionEffect; mutable UsesRemaining: int option; } with
    static member Create (name, effect) = { Name = name; Effect = effect; UsesRemaining = None }
    static member Create (name, effect, uses) = { Name = name; Effect = effect; UsesRemaining = Some uses }

let d20 rollType =
    match rollType with
    | Regular -> d 1 20 0
    | Advantage -> max (d 1 20 0) (d 1 20 0)
    | Disadvantage -> min (d 1 20 0) (d 1 20 0)

// computes the critical hit bonus dice for an attack by stripping off static bonuses
let critBonus (rolls: DieRoll list) =
    rolls |> List.map (fun r -> { r with Plus = 0 })

let formatDice (dice: DieRoll list) =
    let rec loop acc staticBonus = function
        | [] ->
            let mutable dice = []
            for k,v in acc |> Map.toSeq do
                dice <- (sprintf "%dd%d" v k) :: dice
            if staticBonus > 0 then
                dice <- staticBonus.ToString() :: dice
            System.String.Join("+", dice |> List.rev)
        | dr::t ->
            let tst = (acc : Map<int, int>) |> Map.containsKey (dr.DieSize : int)
            let acc' = if Map.containsKey dr.DieSize acc then Map.add dr.DieSize (acc.[dr.DieSize] + dr.N) acc else Map.add dr.DieSize dr.N acc
            loop (if Map.containsKey dr.DieSize acc then Map.add dr.DieSize (acc.[dr.DieSize] + dr.N) acc else Map.add dr.DieSize dr.N acc) (staticBonus + dr.Plus) t
    loop Map.empty 0 dice

type Trait = DefensiveDuelist | MageSlayer | RemarkableAthlete | AthleticsExpertise | AthleticsProficient | ImprovedCritical | ActionSurge | SneakAttack of DieRoll | UncannyDodge
             | HeavyArmorMaster | Reactive | ShieldSpell | FearAura of int | MagicResistant | LuckyDefender | LuckyAttacker | GreatWeaponMaster

let mutable AreaIsObscured = false
module Combatants =
    let statBonus x = if x >= 10 then (x - 10) / 2 else -((11 - x) / 2)

    type Combatant(name, stats) =
        let str, dex, con, int, wis, cha, maxHP = (stats : int * int * int * int * int * int * int)
        let mutable prof = +4 // for this sim, proficiency bonus is +4 for both Slaads and Fighters
        let mutable concentration = ref None
        let mutable hasReaction = true
        let mutable hasActionSurged = false
        let mutable hasAction = true
        let mutable hp = maxHP
        let mutable hasSneakAttacked = false
        let mutable isFearInnoculated = false // once fear is conquered, it's conquered for that whole combat
        let mutable ongoingEffects = []
        let addEffect x = ongoingEffects <- ongoingEffects |> Effect.Add x
        let removeEffect x = ongoingEffects <- ongoingEffects |> Effect.Remove x
        let hasEffect x = ongoingEffects |> Effect.Exists x
        let restoreHP n =
            hp <- min maxHP (hp + n)
        let hasTrait (this: Combatant) = flip List.contains this.Traits
        let acrobatics (this: Combatant) = statBonus dex + (if hasTrait this RemarkableAthlete then prof /2 else 0)
        let athletics (this: Combatant) = statBonus str + (if hasTrait this AthleticsExpertise then prof * 2 elif hasTrait this AthleticsProficient then prof elif hasTrait this RemarkableAthlete then prof /2 else 0)
        let mutable isShielding = false
        let mutable luckUsed =  0
        let mutable hasBonusAction = true
        member this.Prof with get() = prof and set v = prof <- v
        member this.Athletics = athletics this
        member this.InitBonus =
            statBonus dex + (if List.contains RemarkableAthlete this.Traits then prof /2 else 0)
        member this.IsAlive = hp > 0
        member val Name = name
        member val AC = 10 with get, set
        member val Blindsight = false with get, set
        member val Regen = 0 with get, set
        member val Actions = [] with get, set
        member val BonusActions = [] with get, set
        member val Traits = [] with get, set
        member val Resists : DamageType list = [] with get, set
        member val Immunities : DamageType list = [] with get, set
        member val SaveProficiencies : SaveAbility list = [] with get, set
        member this.IsRestrained = hasEffect Restrained
        member private this.TakeDamage n dtype rider =
            if this.Immunities |> List.contains dtype then
                printfn "%s is immune to all %d points of %A damage" this.Name n (dtype)
            else
                let n = if (dtype = Weapon || dtype = MagicalWeapon) && hasTrait this UncannyDodge && this.TryReact() then
                            printfn "Uncanny Dodge! Damaged halved from %d to %d" n (n/2)
                            n / 2
                        else n
                let n = if dtype = Weapon && hasTrait this HeavyArmorMaster then
                            printfn "Heavy Armor Master! Damaged reduced from %d to %d" n (n-3)
                            n - 3
                        else
                            n
                if this.Resists |> List.contains dtype then
                    printfn "%s takes %d points of %A damage! (Halved from %d for %A resistance)" this.Name (n/2) dtype n dtype
                    hp <- hp - n/2
                else
                    printfn "%s takes %d points of %A damage!" this.Name n dtype
                    hp <- hp - n
                match rider with
                | Some(Constrict(_)) ->
                    printfn "%s is restrained!" this.Name
                    addEffect Restrained
                | Some(OngoingDamage(ability, dc, dmg) as ongoing) ->
                    if ongoingEffects |> List.contains(ongoing) |> not then
                        addEffect ongoing
                | None -> ()
                | argMatch -> badMatch __SOURCE_FILE__ __LINE__ argMatch
        member private this.IsProne = hasEffect Prone
        member private this.IsGrappled = hasEffect Grappled
        member private this.IsBlinded = AreaIsObscured && not this.Blindsight
        member this.HP = hp
        member val SP = 0 with get, set
        member this.newRound() =
            hasReaction <- true
            hasAction <- true
            hasBonusAction <- true
            hasSneakAttacked <- false
            isShielding <- false
            if this.Regen > 0 then
                restoreHP this.Regen
        member this.TryReact() =
            if hasReaction then
                hasReaction <- false
                true
            else
                false
        member this.TryShield() =
            if isShielding then
                true
            elif hasTrait this ShieldSpell && this.SP >= 2 && this.TryReact() then
                this.SP <- this.SP - 1
                isShielding <- true
                true
            else
                false
        member this.TryLuck() =
            if luckUsed < 3 then
                luckUsed <- luckUsed + 1
                true
            else
                false
        member this.newTurn() =
            if hasTrait this Reactive then
                hasReaction <- true
        member private this.TryGrapple offensiveRoll =
            let bonus = max (acrobatics this) (athletics this)
            if d20 Regular + bonus < offensiveRoll then
                printfn "%s is grappled!" this.Name
                addEffect Grappled
            else
                printfn "%s avoids grapple." this.Name
        member private this.TryShoveProne offensiveRoll =
            let bonus = max (acrobatics this) (athletics this)
            if d20 Regular + bonus < offensiveRoll then
                printfn "%s is shoved prone!" this.Name
                addEffect Prone
            else
                printfn "%s avoids shove." this.Name
        member private this.SaveBonus ability =
            let p = if (this.SaveProficiencies |> List.contains ability) then this.Prof else 0
            p + (match ability with
                | Str -> str
                | Dex -> dex
                | Con -> con
                | Int -> int
                | Wis -> wis
                | Cha -> cha
                |> statBonus)
        member this.Status =
            let statusEffects = if ongoingEffects.IsEmpty then null else sprintf "(%s)" (System.String.Join(", ", ongoingEffects |> sprintf "%A"))
            if statusEffects <> null then
                sprintf "%s: %d HP (%d damage taken) %s" this.Name hp (maxHP - hp) statusEffects
            else
                sprintf "%s: %d HP (%d damage taken)" this.Name hp (maxHP - hp)
        member this.TakeTurn (target: Combatant) =
            let isProne = ongoingEffects |> Effect.Exists Prone
            let isGrappled = ongoingEffects |> Effect.Exists Grappled
            if isProne && not isGrappled then
                ongoingEffects <- ongoingEffects |> Effect.Remove Prone // stand back up
            match target.Traits |> List.tryFind (function (FearAura(x)) -> true | _ -> false) with
            | Some(FearAura(x)) when not isFearInnoculated ->
                // See if we are afraid/continue to be afraid
                if (d20 Regular + (this.SaveBonus Wis) >= x) then // note: MagicResistant does not apply to fear auras
                    if hasEffect Afraid then
                        removeEffect Afraid
                        printfn "%s recovers from fear of %s" this.Name target.Name
                    else
                        printfn "%s is unfazed by fearsome %s" this.Name target.Name
                    isFearInnoculated <- true
                else
                    addEffect Afraid
            | _ -> ()
            // apply any start-of-turn damaging effects
            for effect in ongoingEffects do
                match effect with
                | OngoingDamage(ability, dc, dmg) ->
                    if (d20 Regular + (this.SaveBonus ability) >= dc) then
                        removeEffect effect
                        printfn "%s resists effect: %A ends" this.Name effect
                    else
                        match dmg with
                        | Unavoidable(dmg, dtype) ->
                            let dmg = DieRoll.eval [dmg]
                            this.TakeDamage dmg dtype None
                        | argMatch -> badMatch __SOURCE_FILE__ __LINE__ argMatch
                | _ -> ()
            let canUse = (fun (a:Action) ->
                match a.Effect with
                | Healing(_) when float this.HP > float maxHP * 0.8 -> false // use healing when at less than 80% of health
                | BreakFree -> isProne && isGrappled
                | _ ->
                    match a.UsesRemaining with | None -> true | Some(x) when x > 0 -> true | _ -> false
            )
            let execute (a: Action) =
                printfn "%s does %s" this.Name a.Name
                if a.UsesRemaining.IsSome then
                    a.UsesRemaining <- Some(a.UsesRemaining.Value - 1)
                let attackerStatus =
                    let hasAdvantage = target.IsProne || target.IsBlinded || target.IsRestrained
                    let hasDisadvantage = ongoingEffects |> List.exists (function | Prone | Restrained | Blinded | Afraid -> true | _ -> false) || this.IsBlinded
                    match hasAdvantage, hasDisadvantage with
                    | true, false -> Advantage
                    | false, true -> Disadvantage
                    | _ -> Regular
                let rec executeAttack a =
                    match a with
                    | Direct(a) ->
                        match a.Rider with
                        | Some (Constrict(dmg)) when target.IsRestrained ->
                            printf "Autohit! %s squeezes %s" this.Name target.Name
                            for (dmg, dtype) in a.Damage do
                                target.TakeDamage (DieRoll.eval dmg) dtype a.Rider
                        | _ ->
                            let attackRoll = d20 attackerStatus
                            printfn "Roll: %d" attackRoll
                            let attackRoll = if (attackRoll = 1 || attackRoll + a.ToHit < target.AC) && hasTrait this LuckyAttacker && this.TryLuck()
                                             then
                                                let reroll = d20 attackerStatus
                                                printfn "%s gets lucky! Re-roll: %d" this.Name reroll
                                                max attackRoll reroll
                                             else
                                                attackRoll
                            let attackRoll = if (attackRoll = 20 || attackRoll + a.ToHit >= target.AC) && hasTrait target LuckyDefender && target.TryLuck()
                                             then
                                                let reroll = d20 attackerStatus
                                                printfn "%s gets lucky! Re-roll: %d" target.Name reroll
                                                min attackRoll reroll
                                             else
                                                attackRoll
                            // add sneak attack damage once per round if SneakAttack trait exists
                            let addSneak dmg =
                                if hasSneakAttacked then
                                    dmg
                                else
                                    match this.Traits |> List.tryFind (function SneakAttack(_) -> true | _ -> false) with
                                    | Some(SneakAttack(sneak)) ->
                                        printfn "SNEAK ATTACK!"
                                        hasSneakAttacked <- true
                                        sneak :: dmg
                                    | _ -> dmg
                            if attackRoll = 20 || (attackRoll >= 19 && hasTrait this ImprovedCritical) then
                                printf "CRITICAL HIT! %s %s %s: " this.Name a.Text target.Name
                                for (dmg, dtype) in a.Damage do
                                    let dmg = DieRoll.eval (List.append dmg (critBonus dmg) |> addSneak)
                                    target.TakeDamage dmg dtype a.Rider
                                if hasTrait this GreatWeaponMaster && hasBonusAction then
                                    hasBonusAction <- false
                                    printfn "Great Weapon Master %s gets a bonus attack!" this.Name
                                    executeAttack (Direct a) // make another attack with bonus action
                            elif attackRoll + a.ToHit >= target.AC then
                                if attackRoll + a.ToHit < (target.AC + target.Prof) && hasTrait target DefensiveDuelist && target.TryReact() then
                                    printfn "%s misses %s (parried)" this.Name target.Name
                                elif attackRoll + a.ToHit < (target.AC + 5) && hasTrait target ShieldSpell && target.TryShield() then
                                    printfn "%s misses %s (Shield)" this.Name target.Name
                                else
                                    printf "Hit! %s %s %s: " this.Name a.Text target.Name
                                    for (dmg, dtype) in a.Damage do
                                        let dmg = DieRoll.eval (dmg |> addSneak)
                                        target.TakeDamage dmg dtype a.Rider
                            else
                                printfn "%s misses %s" this.Name target.Name
                    | BestOf(a1, a2) ->
                        // we invent an ad hoc priority scheme for which attacks are "better": grappling is better against a non-grappled target; then pushing a non-prone target; then whatever attack has the highest expected damage
                        let rec bestOf a1 a2 =
                            // recur is for recursively evaluating args
                            let recur = function | BestOf(lhs, rhs) -> bestOf lhs rhs | x -> x
                            match recur a1, recur a2 with
                            // BestOf cannot happen here because recur has already been called
                            | Grapple, _ | _, Grapple when not target.IsGrappled -> Grapple
                            | ShoveProne, _ | _, ShoveProne when not target.IsProne -> ShoveProne
                            | Direct(_) as lhs, (Direct(_) as rhs) ->
                                let evalDmg targetAC adv att =
                                    match att with
                                    | Direct(att) ->
                                        let hitRate = float (min 19 (max 1 (21 + att.ToHit - targetAC))) / 20.
                                        let hitRate = match adv with | Advantage -> (1. - (1. - hitRate) * (1. - hitRate)) | Disadvantage -> hitRate * hitRate | _ -> hitRate
                                        let critRate = match adv with | Advantage -> (1. - 0.95 * 0.95) | Disadvantage -> 0.05 * 0.05 | _ -> 0.05
                                        let avg = Seq.sumBy (fun (roll : DieRoll) -> float roll.Plus + (float roll.N * float (roll.DieSize + 1) / 2.0))
                                        let dmg = hitRate * (att.Damage |> Seq.sumBy (fst >> avg)) + critRate * (att.Damage |> Seq.sumBy (fst >> critBonus >> avg))
                                        dmg
                                    | argMatch -> badMatch __SOURCE_FILE__ __LINE__ argMatch
                                let evalDmg = evalDmg target.AC attackerStatus
                                if evalDmg(lhs) < evalDmg(rhs) then
                                    rhs
                                else
                                    lhs
                            | Direct(_) as att, _ | _, (Direct(_) as att) -> att
                            | argMatch -> badMatch __SOURCE_FILE__ __LINE__ argMatch
                        let best = bestOf a1 a2
                        executeAttack best
                    | Grapple ->
                        target.TryGrapple (d20 Regular + athletics this)
                    | ShoveProne ->
                        target.TryShoveProne (d20 Regular + athletics this)
                match a.Effect with
                | Attack(attacks) ->
                    for a in attacks do
                        executeAttack a
                | Instant(t, d) -> ()
                | ConcentrationEffect(dc, t, effects) -> ()
                | Healing(amt) -> restoreHP (d amt.N amt.DieSize amt.Plus)
                | BreakFree ->
                    let me = d20 Regular + athletics this
                    let him = d20 Regular + target.Athletics
                    if me > him then
                        printfn "%s breaks free! (%d beats %d)" this.Name me him
                        removeEffect Grappled
                        removeEffect Restrained
                        removeEffect Prone
                    else
                        printfn "%s cannot break free! (%d does not beat %d)" this.Name me him
            if hasAction then
                let action = this.Actions |> Seq.find canUse
                execute action
                if hasTrait this ActionSurge && not hasActionSurged then
                    let action = this.Actions |> Seq.find canUse
                    printfn "ACTION SURGE!!"
                    execute action
                    hasActionSurged <- true
                if hasBonusAction then
                    match this.BonusActions |> Seq.tryFind canUse with
                    | Some(bonus) -> execute bonus
                    | _ -> ()

    let rollInit (c: Combatant) =
            d 1 20 c.InitBonus

    type Combatant with
        member this.rollInit = rollInit this
open Combatants
let undeadNames = ["Allator"; "Ashem"; "Antema"; "Cashal"; "Cernem"; "Korgol"; "Kotyth"; "Losmig"; "Milogos"; "Mithok"; "Murok"; "Simith"; "Styx"; "Terryth"; "Vörnak"; "Sathmog"; "Angmar"; "Khamul"; "Kurgal"; "Mordeith"; "Lothar"; "Nar"; "Ahriman"; "Mogmol"; "Necromorben"; "Mort"; "Zantaron"; "Tahmar"; "Ymic"; "Angrod"; "Uvatha"; "Suleiman"; "Myrkuul"; "Domex"; "Ziruk"; "Sirakzil"; "Ärek Iilum"; "Embaar"; "Maegul"; "Adonhel"; "Karmgul"; "Uftrak"; "Askator"; "Argator"; "Betegrath"; "Bakhtor"; "Bayr al Adhu"; "Cernetu"; "Castartu"; "Darzatu"; "Dimmu"; "Eriadu"; "Ebenezar"; "Fegamur"; "Fechuntu"; "Githeren"; "Gevalodh"; "Gevudrah"; "Gether Kuhadh"; "Hahkaloth"; "Heikhra"; "Igevurdh"; "Igthengu"; "Jithu"; "Jinnud"; "Kumlakh"; "Kmorduk"; "Lodvathu"; "Laidukh"; "Mogracu"; "Mekhaloth"; "Mekhennum"; "Meknadin"; "Mekinthur"; "Meikru Zoth"; "Nur Zaruth"; "Nur Manthu"; "Nardimmu"; "Nakhaloth"; "Oskatu"; "Okeddur"; "Pekharoth"; "Penetor"; "Pantor"; "Qudlik"; "Qudarrath"; "Revdinator"; "Raitor"; "Sidsheku"; "Saoluthi"; "Sarmas"; "Shek Dimmu"; "Trikhatre"; "Trakhtor"; "Uvekhtu"; "Ur"; "Urathu"; "Vekhithu"; "Valgömu"; "Xomanthu"; "Xirrath"; "Ychekhes"; "Ygirod"; "Zrakhnadar"; "Zeginthu"; "Öknar Hadu"; "Öskogoth"; "Mesamalok"; "Alalar"; "Tagg Klatu"; "Melkior"; "Deathtongue"; "Grind"; "Rend"; "Rotheart"; "Deadflesh"; "Wormwood"; "Dustheart"; "Ashsoul"; "Rotflesh"; "Winterbreath"; "Foulflesh"; "Etter"; "Venomspew"; "Plaguevomit"; "Bonecrush"; "Boneater"; "Femur"; "Foetid"; "Gnarlytooth"; "Plaguewhistler"; "Feartongue"; "Leprosy"; "Scrofula"; "Typhon"; "Banewound"; "Eyeless"; "Eyesore"; "Boneheart"; "Foulslay"; "Soulrend"; "Bitterwind"; "Plaguetooth"; "Plaguetongue"; "Rotbrother"; "Heartrot"; "Sigh"; "Boneshudder"; "Shudder"; "Hollow"; "Heartless"; "Headless"; "Crawler"; "Nightbane"; "Wormvenom"; "Wormfood"; "Wormfriend"; "Unburied"; "Childslay"; "Manhate"; "Manslay"; "Shriek"; "Despair"; "Hopevoid"; "Quickdeath"; "Plaguehope"; "Leperlove"; "Sickpit"; "Plaguepit"; "Deathstink"; "Plaguestink"; "Wormlove"; "Wormplague"; "Lepercrawl"; "Spittle"; "Plaguespittle"; "Gravetongue"; "Eyegrave"; "Gravesoul"; "Gravecaller"; "Deathgrip"; "Skullbreaker"; "Graveskull"; "Tombworm"; "Nightworm"; "Nightheart"; "Chillwind"; "Chillsoul"; "Gravechill"; "Gravespittle"; "Tombspittle"; "Plaguestorm"; "Childfeaster"; "Wormfeast"; "Plaguesoul"; "Plaguesinger"; "Deathlight"; "Puswound"; "Puseye"; "Tombshadow"; "Knucklebones"; "Splitskull"; "Screech"; "Graveschreech"; "Orphaneater"; "Fleshfeaster"; "Carnage"; "Dustsoul"; "Deathgasp"; "Deathjest"]
let earthNames = ["Stalagmite"; "Stalactite"; "Basalt"; "Granite"; "Onyx"; "Rockheart"; "Stoneheart"; "Deepheart"; "Pebbleheart"; "Boulderheart"; "Caveheart"; "Basaltheart"; "Earthheart"; "Soilheart"; "Gemheart"; "Onyxheart"; "Paleheart"; "Sandheart"; "Pillarheart"; "Twoheart"; "Rockthought"; "Stonethought"; "Deepthought"; "Pebblethought"; "Boulderthought"; "Cavethought"; "Basaltthought"; "Earththought"; "Soilthought"; "Gemthought"; "Palethought"; "Sandthought"; "Pillarthought"; "Twothought"; "Rocksoul"; "Stonesoul"; "Deepsoul"; "Cavesoul"; "Earthsoul"; "Gemsoul"; "Twosoul"; "Caveson"; "Earthson"; "Twoson"; "Rockstrength"; "Stonestrength"; "Deepstrength"; "Pebblestrength"; "Boulderstrength"; "Cavestrength"; "Earthstrength"; "Twostrength"; "Rockhand"; "Stonehand"; "Pebblehand"; "Boulderhand"; "Cavehand"; "Gemhand"; "Palehand"; "Sandhand"; "Onehand"; "Rockspine"; "Stonespine"; "Pebblespine"; "Boulderspine"; "Cavespine"; "Earthspine"; "Gemspine"; "Pillarspine"; "Twospine"; "Rockmind"; "Stonemind"; "Deepmind"; "Pebblemind"; "Bouldermind"; "Cavemind"; "Earthmind"; "Soilmind"; "Palemind"; "Sandmind"; "Pillarmind"; "Rockfinger"; "Stonefinger"; "Pebblefingers"; "Cavefinger"; "Earthfinger"; "Gemfinger"; "Palefingers"; "Sandfinger"; "Pillarfingers"; "Twofinger"; "Rockbone"; "Stonebone"; "Deepbone"; "Pebblebone"; "Cavebone"; "Earthbone"; "Sandbone"; "Pillarbone"; "Twobone"; "Rockeye"; "Stoneeye"; "Deepeye"; "Pebbleeye"; "Eartheye"; "Sandeye"; "Twoeyes"; "Rockmined"; "Stonemined"; "Pebblemined"; "Cavemined"; "Rockborn"; "Stoneborn"; "Deepborn"; "Pebbleborn"; "Boulderborn"; "Caveborn"; "Basaltborn"; "Earthborn"; "Soilborn"; "Gemborn"; "Paleborn"; "Sandborn"; "Pillarborn"; "Rockspawned"; "Stonespawned"; "Deepspawned"; "Pebblespawned"; "Boulderspawned"; "Cavespawned"; "Earthspawned"; "Soilspawned"; "Gemspawned"; "Sandspawned"; "Pillarspawned"; "Twospawned"; "Rockbreaker"; "Stonebreaker"; "Deepbreaker"; "Pebblebreaker"; "Boulderbreaker"; "Cavebreaker"; "Basaltbreaker"; "Earthbreaker"; "Soilbreaker"; "Gembreaker"; "Onyxbreaker"; "Palebreaker"; "Pillarbreaker"; "Twicebreaker"; "Rubyheart"; "Emeraldheart"; "Saphireheart"; "Diamondheart"; "Rubythought"; "Diamondthought"; "Rubysoul"; "Emeraldsoul"; "Diamondsoul"; "Rubymind"; "Emeraldmind"; "Diamondmind"; "Rubyeye"; "Emeraldeye"; "Diamondeye"; "Rubyborn"; "Emeraldborn"; "Saphireborn"; "Diamondborn"; "Rubyspawned"; "Emeraldspawned"; "Saphirespawned"; "Diamondspawned"; "Ruby"; "Emerald"; "Saphire"; "Diamond"; "Stonekiss"; "Pebblekiss"; "Cavekiss"; "Earthkiss"; "Sandkiss"; "Twokiss"; "Olmkiss"; "Rockfriend"; "Stonefriend"; "Deepfriend"; "Pebblefriend"; "Boulderfriend"; "Cavefriend"; "Basaltfriend"; "Earthfriend"; "Soilfriend"; "Gemfriend"; "Onyxfriend"; "Palefriend"; "Sandfriend"; "Pillarfriend"; "Twofriend"; "Olmfriend"]
let humanNames = ["Ruprecht";"John";"Elias";"Katie";"Kitty";"Lux";"Grizzabella";"Graal Tiger";"Platt";"Rupert Grint";"Daniel Pinkwater";"Maid Marian";"Robin Hood"]
let giantNames = ["Gronk";"Dank";"Splatt";"Mudd";"Platt";"Ootini";"Grrrronk";"Dredd"]
let nameOf (lst: _ list) title =
    sprintf "%s the %s" lst.[r.Next(lst.Length)] title
let scuzName() =
    nameOf undeadNames "Death Scuz"
let earthName() = nameOf earthNames "Earthling"
let banditName() = nameOf humanNames "Bandit Captain"
let ogreName() = nameOf giantNames "Ogre"

let deathScuzz() = Combatant(scuzName(), (20, 15, 19, 15, 10, 18, 170), AC=18, Regen=10, Blindsight=true, Resists = [Fire],
                     Actions = [
//                        Action.Create("Cloudkill", ConcentrationEffect(15, All, [Blinded; Damage(SaveForHalf(Con, 15, DieRoll.Create(5,8), Poison))]), 1)
//                        Action.Create("Fear", ConcentrationEffect(15, EnemyOnly, [Afraid]), 2)
                        Action.Create("Multiattack", Attack [
                                                                Attack.CreateMagic "bites" 9 [DieRoll.Create(1, 8, 5); DieRoll.Create(2, 6)]
                                                                Attack.CreateMagic "cuts" 9 [DieRoll.Create(2, 6, 5); DieRoll.Create(2, 6)]
                                                                Attack.CreateMagic "cuts" 9 [DieRoll.Create(2, 6, 5); DieRoll.Create(2, 6)]
                                                                ])
//                        Action.Create("Fireball", Instant(All, SaveForHalf(Dex, 15, DieRoll.Create(8, 6), Fire)), 2)
                     ])

let earthElemental() = Combatant(earthName(), (20, 8, 20, 5, 10, 5, 126), AC=17, Resists = [Weapon],
                         Actions = [
                            Action.Create("Multiattack", Attack [
                                                                    Attack.Create "slams" 8 [DieRoll.Create(2, 8, 5)]
                                                                    Attack.Create "slams" 8 [DieRoll.Create(2, 8, 5)]
                                                                    ])
                         ])

let banditCaptain() = Combatant(banditName(), (15, 16, 14, 14, 11, 14, 65), AC=15, Traits = [DefensiveDuelist], Prof = +2,
                            Actions = [Action.Create("Melee attack",
                                            Attack [
                                                Attack.Create "slashes" 5 [DieRoll.Create(1, 6, 3)]
                                                Attack.Create "slashes" 5 [DieRoll.Create(1, 6, 3)]
                                                Attack.Create "stabs" 5 [DieRoll.Create(1, 4, 3)]
                                                ])
                            ])

// same as bandit captain but with fewer HP
let weakenedBanditCaptain() = Combatant(banditName(), (15, 16, 14, 14, 11, 14, 39), AC=15,
                                Traits = [DefensiveDuelist], Prof = +2,
                                Actions = [Action.Create("Melee attack",
                                                Attack [
                                                    Attack.Create "slashes" 5 [DieRoll.Create(1, 6, 3)]
                                                    Attack.Create "slashes" 5 [DieRoll.Create(1, 6, 3)]
                                                    Attack.Create "stabs" 5 [DieRoll.Create(1, 4, 3)]
                                                    ])
                                ])

// champion1 was generated using PHB standard array and variant human Fighter 1 with Defense Style and chain mail + shield + longsword and Heavy Armor Master feat
let champion1() = Combatant(nameOf humanNames "Proto-Champion Brawler", (16, 14, 14, 10, 12, 8, 12), AC=19,
                            Traits=[HeavyArmorMaster], Prof = +2,
                            Actions = [Action.Create("Melee attack",
                                            Attack [
                                                Attack.Create "slashes" 5 [DieRoll.Create(1, 8, 3)]
                                                ])
                                       ],
                            BonusActions = [
                                Action.Create("Second Wind", Healing (DieRoll.Create(1, 10, 1)), 1)
                            ])
// archer1 was generated using PHB standard array and variant human Fighter 1 with Archery Style and studded leather and heavy crossbow and Sharpshooter feat
let archer1() = Combatant(nameOf humanNames "Proto-Champion Archer", (14, 16, 14, 10, 12, 8, 12), AC=15, Prof = +2,
                            Actions = [Action.Create("Shoot",
                                            Attack [
                                                Attack.BestOf(Attack.Create "shoots" 7 [DieRoll.Create(1, 10, 3)], Attack.Create "headshots" 2 [DieRoll.Create(1, 10, 13)])
                                                ])
                                        ],
                            BonusActions = [
                                Action.Create("Second Wind", Healing (DieRoll.Create(1, 10, 1)), 1)
                            ])

// champion5 was generated using PHB standard array and variant human Fighter 5 with Dueling Style and plate armor + shield + nonmagical longsword and Heavy Armor Master feat
let champion5() = Combatant(nameOf humanNames "Champion", (19, 14, 14, 10, 12, 8, 44), AC=20,
                            Traits=[HeavyArmorMaster; ActionSurge; AthleticsProficient; ImprovedCritical], Prof = +3,
                            Actions = [Action.Create("Melee attack",
                                            Attack [
                                                Attack.Create "slashes" 7 [DieRoll.Create(1, 8, 4)]
                                                Attack.Create "slashes" 7 [DieRoll.Create(1, 8, 4)]
                                                ])
                                       ],
                            BonusActions = [
                                Action.Create("Second Wind", Healing (DieRoll.Create(1, 10, 5)), 1)
                            ])
// archer5 was generated using PHB standard array and variant human Fighter 5 with Archery Style and studded leather and nonmagical longbow +0 and Sharpshooter feat
let archer5() = Combatant(nameOf humanNames "Archer", (14, 18, 14, 10, 12, 8, 44), AC=16, Prof = +3,
                            Traits=[ActionSurge; AthleticsProficient; ImprovedCritical],
                            Actions = [Action.Create("Shoot",
                                            Attack [
                                                Attack.BestOf(Attack.Create "shoots" 9 [DieRoll.Create(1, 8, 4)], Attack.Create "headshots" 4 [DieRoll.Create(1, 8, 14)])
                                                Attack.BestOf(Attack.Create "shoots" 9 [DieRoll.Create(1, 8, 4)], Attack.Create "headshots" 4 [DieRoll.Create(1, 8, 14)])
                                                ])
                                        ],
                            BonusActions = [
                                Action.Create("Second Wind", Healing (DieRoll.Create(1, 10, 5)), 1)
                            ])

// champion9 was generated using PHB standard array and variant human Fighter 5 with Dueling Style and plate armor + shield + nonmagical longsword and Heavy Armor Master feat
let champion9() = Combatant(nameOf humanNames "Champion", (20, 14, 14, 10, 11, 8, 76), AC=20,
                            Traits=[HeavyArmorMaster; ActionSurge; AthleticsProficient; ImprovedCritical], Prof = +4,
                            Actions = [Action.Create("Melee attack",
                                            Attack [
                                                Attack.Create "slashes" 9 [DieRoll.Create(1, 8, 5)]
                                                Attack.Create "slashes" 9 [DieRoll.Create(1, 8, 5)]
                                                ])
                                       ],
                            BonusActions = [
                                Action.Create("Second Wind", Healing (DieRoll.Create(1, 10, 9)), 1)
                            ])
// archer9 was generated using PHB standard array and variant human Fighter 5 with Archery Style and studded leather and nonmagical longbow and Sharpshooter feat
let archer9() = Combatant(nameOf humanNames "Archer", (14, 20, 14, 10, 12, 8, 76), AC=17, Prof = +4,
                            Traits=[ActionSurge; AthleticsProficient; ImprovedCritical],
                            Actions = [Action.Create("Shoot",
                                            Attack [
                                                Attack.BestOf(Attack.Create "shoots" 11 [DieRoll.Create(1, 8, 5)], Attack.Create "headshots" 6 [DieRoll.Create(1, 8, 15)])
                                                Attack.BestOf(Attack.Create "shoots" 11 [DieRoll.Create(1, 8, 5)], Attack.Create "headshots" 6 [DieRoll.Create(1, 8, 15)])
                                                ])
                                        ],
                            BonusActions = [
                                Action.Create("Second Wind", Healing (DieRoll.Create(1, 10, 9)), 1)
                            ])

// champion9b was generated using PHB standard array and variant human Fighter 9 with Dueling Style and plate armor + shield + longsword +1 and Heavy Armor Master + Tough feats
let champion9b() = Combatant(nameOf humanNames "Champion", (20, 14, 14, 10, 11, 8, 94), AC=18,
                            Traits=[HeavyArmorMaster; ActionSurge; AthleticsProficient; ImprovedCritical], Prof = +4,
                            Actions = [Action.Create("Melee attack",
                                            Attack [
                                                Attack.CreateMagic "slashes" 10 [DieRoll.Create(1, 8, 6)]
                                                Attack.CreateMagic "slashes" 10 [DieRoll.Create(1, 8, 6)]
                                                ])
                                       ],
                            BonusActions = [
                                Action.Create("Second Wind", Healing (DieRoll.Create(1, 10, 9)), 1)
                            ])
// archer9b was generated using PHB standard array and variant human Fighter 9 with Archery Style and studded leather and longbow +1 and Sharpshooter + Tough feats
let archer9b() = Combatant(nameOf humanNames "Archer", (14, 20, 14, 10, 12, 8, 94), AC=17, Prof = +4,
                            Traits=[ActionSurge; AthleticsProficient; ImprovedCritical],
                            Actions = [Action.Create("Shoot",
                                            Attack [
                                                Attack.BestOf(Attack.CreateMagic "shoots" 12 [DieRoll.Create(1, 8, 6)], Attack.CreateMagic "headshots" 7 [DieRoll.Create(1, 8, 16)])
                                                Attack.BestOf(Attack.CreateMagic "shoots" 12 [DieRoll.Create(1, 8, 6)], Attack.CreateMagic "headshots" 7 [DieRoll.Create(1, 8, 16)])
                                                ])
                                        ],
                            BonusActions = [
                                Action.Create("Second Wind", Healing (DieRoll.Create(1, 10, 9)), 1)
                            ])

// champion8b was generated using PHB standard array and variant human Fighter 5 with Dueling Style and plate armor + shield + longsword +1 and Heavy Armor Master feat
let champion8b() = Combatant(nameOf humanNames "Champion", (20, 14, 14, 10, 11, 8, 68), AC=18,
                            Traits=[HeavyArmorMaster; ActionSurge; AthleticsProficient; ImprovedCritical], Prof = +3,
                            Actions = [Action.Create("Melee attack",
                                            Attack [
                                                Attack.CreateMagic "slashes" 9 [DieRoll.Create(1, 8, 6)]
                                                Attack.CreateMagic "slashes" 9 [DieRoll.Create(1, 8, 6)]
                                                ])
                                       ],
                            BonusActions = [
                                Action.Create("Second Wind", Healing (DieRoll.Create(1, 10, 8)), 1)
                            ])
// archer8b was generated using PHB standard array and variant human Fighter 5 with Archery Style and studded leather and longbow +1 and Sharpshooter feat
let archer8b() = Combatant(nameOf humanNames "Archer", (14, 20, 14, 10, 12, 8, 68), AC=17, Prof = +3,
                            Traits=[ActionSurge; AthleticsProficient; ImprovedCritical],
                            Actions = [Action.Create("Shoot",
                                            Attack [
                                                Attack.BestOf(Attack.CreateMagic "shoots" 11 [DieRoll.Create(1, 8, 6)], Attack.CreateMagic "headshots" 6 [DieRoll.Create(1, 8, 16)])
                                                Attack.BestOf(Attack.CreateMagic "shoots" 11 [DieRoll.Create(1, 8, 6)], Attack.CreateMagic "headshots" 6 [DieRoll.Create(1, 8, 16)])
                                                ])
                                        ],
                            BonusActions = [
                                Action.Create("Second Wind", Healing (DieRoll.Create(1, 10, 8)), 1)
                            ])


// rogue1 was generated using PHB standard array and variant human Rogue 1 with studded leather and light crossbow and Skulker feat (doesn't factor into this fight)
let rogue1() = Combatant(nameOf humanNames "Rogue", (10, 16, 14, 14, 12, 8, 12), AC=15, Prof = +2, Traits=[SneakAttack(DieRoll.Create(1, 6))],
                            Actions = [Action.Create("Shoot",
                                            Attack [
                                                Attack.Create "shoots" 5 [DieRoll.Create(1, 10, 3)]
                                                ])
                                        ]
                            )

let ogre() = Combatant(ogreName(), (19, 8, 16, 5, 7, 7, 59), AC=11, Traits = [], Prof = +2,
                            Actions = [Action.Create("Club attack",
                                            Attack [
                                                Attack.Create "smashes" 6 [DieRoll.Create(2, 8, 4)]
                                                ])
                            ])

let marilith() = Combatant(nameOf undeadNames "Marilith", (18, 20, 20, 18, 16, 20, 189), AC=18, Traits = [DefensiveDuelist;Reactive], Resists = [Weapon], Prof = +5,
                            Actions = [Action.Create("Multiattack",
                                            Attack [
                                                Attack.CreateMagicWithRider "constricts" 9 [DieRoll.Create(2, 10, 4)] (Constrict (DieRoll.Create(2, 10, 4)))
                                                Attack.CreateMagic "cuts" 9 [DieRoll.Create(2, 8, 4)]
                                                Attack.CreateMagic "cuts" 9 [DieRoll.Create(2, 8, 4)]
                                                Attack.CreateMagic "cuts" 9 [DieRoll.Create(2, 8, 4)]
                                                Attack.CreateMagic "cuts" 9 [DieRoll.Create(2, 8, 4)]
                                                Attack.CreateMagic "cuts" 9 [DieRoll.Create(2, 8, 4)]
                                                Attack.CreateMagic "cuts" 9 [DieRoll.Create(2, 8, 4)]
                                                ])
                            ])

// Rufus was created using PHB standard array (15 14 13 12 10 8), variant human Champion 12, with feats Sharpshooter, Crossbow Expert, and Tough; fighting styles Archery and Defense. Has a +1 Hand Crossbow.
let shooter() = Combatant("Rufus the Archer", (12, 20, 14, 10, 14, 8, 124), AC=19, Traits = [RemarkableAthlete; ImprovedCritical; ActionSurge; AthleticsProficient],
                    Actions = [
                        Action.Create("Crossbow Attack", Attack [
                                                                BestOf (Attack.CreateMagic "headshots" 7 [DieRoll.Create(1, 6, 16)], Attack.CreateMagic "shoots" 12 [DieRoll.Create(1, 6, 6)])
                                                                BestOf (Attack.CreateMagic "headshots" 7 [DieRoll.Create(1, 6, 16)], Attack.CreateMagic "shoots" 12 [DieRoll.Create(1, 6, 6)])
                                                                BestOf (Attack.CreateMagic "headshots" 7 [DieRoll.Create(1, 6, 16)], Attack.CreateMagic "shoots" 12 [DieRoll.Create(1, 6, 6)])
                                                                ])
                        Action.Create("Rapier Attack", Attack [
                                                                BestOf (Grapple, BestOf(ShoveProne, Attack.CreateMagic "stabs" 10 [DieRoll.Create(1, 6, 6)]))
                                                                BestOf (Grapple, BestOf(ShoveProne, Attack.CreateMagic "stabs" 10 [DieRoll.Create(1, 6, 6)]))
                                                                BestOf (Grapple, BestOf(ShoveProne, Attack.CreateMagic "stabs" 10 [DieRoll.Create(1, 6, 6)]))
                                                                ])
                    ],
                    BonusActions = [
                        Action.Create("Second Wind", Healing (DieRoll.Create(1, 10, 12)), 1)
                        Action.Create("Bonus Attack", Attack [BestOf (Attack.Create "headshots" 6 [DieRoll.Create(1, 8, 15)], Attack.Create "shoots" 11 [DieRoll.Create(1, 8, 5)])])
                    ])


// Brutus was created using PHB standard array (15 14 13 12 10 8), variant human Champion 12, with feats Defensive Duelist, Mage Slayer, and Tough; fighting styles Dueling and Defense. Has a +1 Rapier.
let stabber() = Combatant("Brutus the Tank", (20, 12, 14, 10, 14, 8, 124), AC=19, Traits = [DefensiveDuelist; MageSlayer; RemarkableAthlete; AthleticsProficient; ImprovedCritical; ActionSurge],
                    Actions = [
                        Action.Create("Attack", Attack [
                                                                BestOf (Grapple, BestOf(ShoveProne, Attack.CreateMagic "stabs" 10 [DieRoll.Create(1, 8, 8)]))
                                                                BestOf (Grapple, BestOf(ShoveProne, Attack.CreateMagic "stabs" 10 [DieRoll.Create(1, 8, 8)]))
                                                                BestOf (Grapple, BestOf(ShoveProne, Attack.CreateMagic "stabs" 10 [DieRoll.Create(1, 8, 8)]))
                                                                ])
                    ],
                    BonusActions = [
                        Action.Create("Second Wind", Healing (DieRoll.Create(1, 10, 12)), 1)
                    ])

// D'Artagnan was created using PHB standard array (15 14 13 12 10 8), variant human Champion 5/Swashbuckler 7, with feat Sharpshooter (not used in this combat); and fighting style Archery. Has a +1 Longbow which isn't used in this fight and a +1 rapier.
let swash() = Combatant("D'Artagnan the Swashbuckler", (12, 20, 14, 10, 14, 8, 124), AC=18, Traits = [ImprovedCritical; ActionSurge; AthleticsExpertise; SneakAttack(DieRoll.Create(4, 6)); UncannyDodge],
                    Actions = [
                        Action.Create("Attack", Attack [
                                                                BestOf (Grapple, BestOf(ShoveProne, Attack.CreateMagic "stabs" 10 [DieRoll.Create(1, 8, 6)]))
                                                                BestOf (Grapple, BestOf(ShoveProne, Attack.CreateMagic "stabs" 10 [DieRoll.Create(1, 8, 6)]))
                                                                ])
                    ],
                    BonusActions = [
                        Action.Create("Second Wind", Healing (DieRoll.Create(1, 10, 5)), 1)
                    ])

let githyankiKnight() = Combatant(nameOf ["Gith";"Valjeezra";"Aliera";"Morrolan";"Sethrik"] "Knight of Gith", (16, 14, 15, 14, 14, 15, 91), AC=18,
                            Actions = [
                                Action.Create("Multiattack",
                                    Attack [
                                        Attack.CreateMagic "cuts" 9 [DieRoll.Create(2, 6, 6); DieRoll.Create(3, 6)]
                                        Attack.CreateMagic "cuts" 9 [DieRoll.Create(2, 6, 6); DieRoll.Create(3, 6)]
                                    ])
                            ])
let zerth() = Combatant(nameOf ["Zerthimon"; "Kolin"; "Praxis"; "Roneesi"; "Katrin"] "Zerth", (13, 18, 15, 16, 17, 12, 84), AC=17,
                        Traits = [ShieldSpell], SP=6,
                        Actions = [
                            Action.Create("Multiattack",
                                Attack [
                                    Attack.CreateMagic "punches" 7 [DieRoll.Create(2, 6, 4); DieRoll.Create(3, 8)]
                                    Attack.CreateMagic "kicks" 7 [DieRoll.Create(2, 6, 4); DieRoll.Create(3, 8)]
                                ])
                        ])

let githyanki() = Combatant(nameOf ["Gith";"Valjeezra";"Aliera";"Morrolan";"Sethrik"] "githyanki", (15, 14, 12, 13, 13, 10, 49), AC=17,
                            Actions = [
                                Action.Create("Multiattack",
                                    Attack [
                                        Attack.CreateMagic "cuts" 4 [DieRoll.Create(2, 6, 2); DieRoll.Create(2, 6)]
                                        Attack.CreateMagic "cuts" 4 [DieRoll.Create(2, 6, 2); DieRoll.Create(2, 6)]
                                    ])
                            ])
let githzerai() = Combatant(nameOf ["Zerthimon"; "Kolin"; "Praxis"; "Roneesi"; "Katrin"] "githzerai", (12, 15, 12, 13, 14, 10, 38), AC=14,
                        Traits = [ShieldSpell], SP=6,
                        Actions = [
                            Action.Create("Multiattack",
                                Attack [
                                    Attack.CreateMagic "punches" 4 [DieRoll.Create(1, 8, 2); DieRoll.Create(2, 8)]
                                    Attack.CreateMagic "kicks" 4 [DieRoll.Create(1, 8, 2); DieRoll.Create(2, 8)]
                                ])
                        ])

let balor() = Combatant(nameOf ["Kolor"; "Koloss"; "Zentradi"; "Abnaxis"; "Gargamontor"] "Balor", (26, 15, 22, 20, 16, 22, 262), Prof = +6, SaveProficiencies=[Str;Wis;Con;Cha],
                        AC = 19, Resists = [Lightning; Cold; Weapon], Immunities = [Fire; Poison], Blindsight = true,
                        Traits = [MagicResistant],
                        Actions = [
                            Action.Create("Multiattack",
                                Attack [
                                    Attack.CreateDualType "slashes" 14 ([DieRoll.Create(3,8,8)], MagicalWeapon) ([DieRoll.Create(3,8)], Lightning)
                                    Attack.CreateDualType "whips" 14 ([DieRoll.Create(2,6,8)], MagicalWeapon) ([DieRoll.Create(3,6)], Fire)
                                ])
                        ])

let balorCeltavian() = Combatant(nameOf ["Kolor"; "Koloss"; "Zentradi"; "Abnaxis"; "Gargamontor"] "Balor", (26, 15, 22, 20, 16, 22, 393), Prof = +6, SaveProficiencies=[Str;Wis;Con;Cha],
                        AC = 19, Resists = [Lightning; Cold; Weapon], Immunities = [Fire; Poison], Blindsight = true,
                        Traits = [MagicResistant],
                        Actions = [
                            Action.Create("Multiattack",
                                Attack [
                                    Attack.CreateDualType "slashes" 14 ([DieRoll.Create(3,8,8)], MagicalWeapon) ([DieRoll.Create(3,8)], Lightning)
                                    Attack.CreateDualType "whips" 14 ([DieRoll.Create(2,6,8)], MagicalWeapon) ([DieRoll.Create(3,6)], Fire)
                                ])
                        ])

let pitFiend() = Combatant(nameOf ["Abariel";"Benetor";"Benedict";"Imariel";"Abbadon";"Lucifer"] "Pit Fiend", (26, 14, 24, 22, 18, 24, 300), Prof = +6, SaveProficiencies=[Dex;Con;Wis],
                        AC = 19, Resists = [Cold; Weapon], Immunities = [Fire; Poison], Blindsight = true,
                        Traits = [MagicResistant;FearAura(21)],
                        Actions = [
                            Action.Create("Multiattack",
                                Attack [
                                    Attack.CreateMagicWithRider "bites" 14 [DieRoll.Create(4, 6, 8)] (OngoingDamage(Con, 21, Unavoidable(DieRoll.Create(6, 6), Poison)))
                                    Attack.CreateMagic "claws" 14 [DieRoll.Create(2, 8, 8)]
                                    Attack.CreateDualType "smashes" 14 ([DieRoll.Create(2, 8, 8)], MagicalWeapon) ([DieRoll.Create(6,6)], Fire)
                                    Attack.CreateMagic "smacks" 14 [DieRoll.Create(3, 10, 8)]
                                ])
                        ])

let orc() = Combatant(nameOf giantNames "orc", (16, 12, 16, 7, 11, 10, 15), Prof = +2, AC = 13,
                        Actions = [
                            Action.Create("Attack", Attack [Attack.Create "cuts" 5 [DieRoll.Create(1, 12, 3)]])
                        ])

let heavyArmor1() = Combatant(nameOf humanNames "Valerian", (16, 12, 16, 11, 12, 10, 13), Prof = +2, AC = 19,
                        Traits=[HeavyArmorMaster],
                        Actions = [
                            Action.Create("Attack", Attack [Attack.Create "cuts" 5 [DieRoll.Create(1, 8, 3)]])
                        ],
                        BonusActions = [
                            Action.Create("Second Wind", Healing (DieRoll.Create(1, 10, 1)), 1)
                        ])
let gwm1() = Combatant(nameOf humanNames "Cimmerian", (16, 12, 16, 11, 12, 10, 13), Prof = +2, AC = 17,
                        Traits=[GreatWeaponMaster],
                        Actions = [
                            Action.Create("Attack", Attack [BestOf(Attack.Create "slices" 5 [DieRoll.Create(1, 8, 3)], Attack.Create "slices" 0 [DieRoll.Create(1, 8, 13)])])
                        ],
                        BonusActions = [
                            Action.Create("Second Wind", Healing (DieRoll.Create(1, 10, 1)), 1)
                        ])
let crossbowExpert1() = Combatant(nameOf humanNames "crossbow expert", (12, 16, 16, 11, 12, 10, 13), Prof = +2, AC = 16,
                            Actions = [
                                Action.Create("Attack", Attack [Attack.Create "shoots" 7 [DieRoll.Create(1, 10, 3)]])
                            ],
                            BonusActions = [
                                Action.Create("Attack", Attack [Attack.Create "shoots" 7 [DieRoll.Create(1, 6, 3)]])
                            ])
let lucky1() = Combatant(nameOf humanNames "Fortunate", (16, 12, 16, 11, 12, 10, 13), Prof = +2, AC = 19,
                        Traits=[LuckyDefender],
                        Actions = [
                            Action.Create("Attack", Attack [Attack.Create "cuts" 5 [DieRoll.Create(1, 8, 3)]])
                        ],
                        BonusActions = [
                            Action.Create("Second Wind", Healing (DieRoll.Create(1, 10, 1)), 1)
                        ])
let lucky1b() = Combatant(nameOf humanNames "Fortunate", (16, 12, 16, 11, 12, 10, 13), Prof = +2, AC = 19,
                        Traits=[LuckyDefender;LuckyAttacker],
                        Actions = [
                            Action.Create("Attack", Attack [Attack.Create "cuts" 5 [DieRoll.Create(1, 8, 3)]])
                        ],
                        BonusActions = [
                            Action.Create("Second Wind", Healing (DieRoll.Create(1, 10, 1)), 1)
                        ])
let swash1() = Combatant(nameOf humanNames "Agile", (16, 12, 16, 11, 12, 10, 13), Prof = +2, AC = 19,
                        Traits=[DefensiveDuelist],
                        Actions = [
                            Action.Create("Attack", Attack [Attack.Create "cuts" 5 [DieRoll.Create(1, 8, 3)]])
                        ],
                        BonusActions = [
                            Action.Create("Second Wind", Healing (DieRoll.Create(1, 10, 1)), 1)
                        ])


let fight side1 side2 =
    let all = List.append side1 side2
    let computeOrder () =
        // crudely determine initiative order--break ties in arbitrary fashion
        all |> List.map (fun (x: Combatant) -> x.rollInit, x) |> List.sortByDescending fst |> List.map snd
    let combatantsInOrder = computeOrder() // re-assign in initiative order
    let takeTurn (c: Combatant) (t: Combatant) =
        ()
    let printStatus (c: Combatant) =
        printfn "%s" (c.Status)
    let hasLive side =
        side |> List.exists (fun (c: Combatant) -> c.IsAlive)
    let findOpponent c =
        let opponents = if List.exists ((=)c) side1 then side2 else side1
        opponents |> List.tryFind(fun (c: Combatant) -> c.IsAlive)
    let mutable round = 1
    while hasLive side1 && hasLive side2 do
        printfn "Round #%d:" round
        for c in combatantsInOrder do
            if c.IsAlive then
                match findOpponent c with
                | Some(opponent) -> c.TakeTurn opponent
                | None -> ()
            all |> List.iter (fun c -> c.newTurn())
        printfn "Status after round #%d:" round
        all |> List.iter printStatus
        all |> List.iter (fun c -> c.newRound())
        round <- round + 1

let compare opponents friendlyAlternatives =
    let NumberOfRuns = 100
    let avgs = [
        for alt in friendlyAlternatives do
            let results =
                [for x in 1..NumberOfRuns do
                    let friend : Combatant = alt()
                    let foes = opponents |> List.map (fun x -> x())
                    printfn "========================\n"
                    fight [friend] foes
                    yield friend.IsAlive, friend.HP
                    ]
            let live = results |> List.filter fst
            let avgHp = ((live |> List.sumBy snd |> float) / (float NumberOfRuns))
            let friend = alt()
            let foes = System.String.Join(" and ", opponents |> List.map (fun x -> x().Name))
            yield sprintf "%s wins %d out of %d matches against %s, with %.2f HP remaining (%d%% of total)" friend.Name (live |> List.length) NumberOfRuns foes avgHp (avgHp / float friend.HP * 100. |> int)
        ]
    for report in avgs do
        printfn "%s" report

let evalGroup opponents friendlies =
    let NumberOfRuns = 100
    let results =
        [for x in 1..NumberOfRuns do
            let friends : Combatant list = friendlies |> List.map (fun f -> f())
            let foes = opponents |> List.map (fun x -> x())
            printfn "========================\n"
            fight friends foes
            yield (friends |> List.exists (fun (c: Combatant) -> c.IsAlive)), (friends |> List.filter (fun (c:Combatant) -> c.IsAlive) |> List.length)
            ]
    let live = results |> List.filter fst
    let avgStillAlive = ((live |> List.sumBy snd |> float) / (float NumberOfRuns))
    let friends = System.String.Join(" and ", friendlies |> List.map (fun x -> (x() : Combatant).Name))
    let foes = System.String.Join(" and ", opponents |> List.map (fun x -> x().Name))
    let averageSurviving = ((live |> List.map snd |> List.sum |> float) / float (List.length live))
    printfn "%s win %d out of %d matches against %s, with %.2f members still alive on average" friends (live |> List.length) NumberOfRuns foes averageSurviving

//compare [githyankiKnight] [zerth]
//compare [githyanki] [githzerai]
//compare [githyankiKnight] [zerth]
//evalGroup [zerth;zerth;zerth] [githyankiKnight;githyankiKnight;githyankiKnight]
//evalGroup [githzerai;githzerai;zerth] [githyanki;githyanki;githyankiKnight]
//fight [balor()] [pitFiend()]
//compare [champion9b;champion9b;archer9b;archer9b;archer9b] [pitFiend]
//compare [champion9b;champion9b;archer9b;archer9b;archer9b] [balor]

fight [lucky1()] [orc()]
compare [orc] [heavyArmor1;lucky1;lucky1b;swash1;gwm1;crossbowExpert1]