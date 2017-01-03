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
type DirectAttack = { Text: string; ToHit: int; Damage: DieRoll list }
type Attack = Direct of DirectAttack | Grapple | ShoveProne | BestOf of Attack * Attack with
    static member Create descriptor t d = Direct { Text = descriptor; ToHit = t; Damage = d }
type AoETarget = All | EnemyOnly
type DamageType = Weapon | Poison | Fire
type SaveAbility = Dex | Con
type Damage = Unavoidable of DieRoll * DamageType | SaveForHalf of SaveAbility * int * DieRoll * DamageType
type Effect = Afraid | Blinded | Damage of Damage
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

let mutable AreaIsObscured = false
module Combatants =
    let statBonus x = if x >= 10 then (x - 10) / 2 else -((11 - x) / 2)

    type Combatant(name, stats) =
        let str, dex, con, int, wis, cha, maxHP = (stats : int * int * int * int * int * int * int)
        let mutable prof = +4 // for this sim, proficiency bonus is +4 for both Slaads and Fighters
        let mutable isProne = false
        let mutable isGrappled = false
        let mutable isAfraid = false
        let mutable concentration = ref None
        let mutable hasReaction = true
        let mutable hasActionSurged = false
        let mutable hasAction = true
        let mutable hp = maxHP
        let mutable hasSneakAttacked = false
        let restoreHP n =
            hp <- min maxHP (hp + n)
        let hasTrait (this: Combatant) = flip List.contains this.Traits
        let acrobatics (this: Combatant) = statBonus dex + (if hasTrait this RemarkableAthlete then prof /2 else 0)
        let athletics (this: Combatant) = statBonus str + (if hasTrait this AthleticsExpertise then prof * 2 elif hasTrait this AthleticsProficient then prof elif hasTrait this RemarkableAthlete then prof /2 else 0)
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
        member private this.TakeDamage n dtype =
            let n = if dtype = Weapon && hasTrait this UncannyDodge && this.TryReact() then
                        printfn "Uncanny Dodge! Damaged halved from %d to %d" n (n/2)
                        n / 2
                    else n
            if this.Resists |> List.contains dtype then
                printfn "%s takes %d points of damage! (Halved from %d for %A resistance)" this.Name (n/2) n dtype
                hp <- hp - n/2
            else
                printfn "%s takes %d points of damage!" this.Name n
                hp <- hp - n
        member private this.IsProne = isProne
        member private this.IsGrappled = isGrappled
        member private this.IsBlinded = AreaIsObscured && not this.Blindsight
        member this.HP = hp
        member this.newRound() =
            hasReaction <- true
            hasAction <- true
            hasSneakAttacked <- false
            if this.Regen > 0 then
                restoreHP this.Regen
        member this.TryReact() =
            if hasReaction then
                hasReaction <- false
                true
            else
                false
        member private this.TryGrapple offensiveRoll =
            let bonus = max (acrobatics this) (athletics this)
            if d20 Regular + bonus < offensiveRoll then
                printfn "%s is grappled!" this.Name
                isGrappled <- true
            else
                printfn "%s avoids grapple." this.Name
        member private this.TryShoveProne offensiveRoll =
            let bonus = max (acrobatics this) (athletics this)
            if d20 Regular + bonus < offensiveRoll then
                printfn "%s is shoved prone!" this.Name
                isProne <- true
            else
                printfn "%s avoids shove." this.Name
        member this.Status =
            let statusEffects = if isGrappled && isProne then "(grappled, prone)" elif isGrappled then "(grappled)" elif isProne then "(prone)" else null
            if statusEffects <> null then
                sprintf "%s: %d HP (%d damage taken) %s" this.Name hp (maxHP - hp) statusEffects
            else
                sprintf "%s: %d HP (%d damage taken)" this.Name hp (maxHP - hp)
        member this.TakeTurn (target: Combatant) =
            if isProne && not isGrappled then
                isProne <- false // stand back up
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
                    let hasAdvantage = target.IsProne || target.IsBlinded
                    let hasDisadvantage = this.IsProne || this.IsBlinded || isAfraid
                    match hasAdvantage, hasDisadvantage with
                    | true, false -> Advantage
                    | false, true -> Disadvantage
                    | _ -> Regular
                let rec executeAttack a =
                    match a with
                    | Direct(a) ->
                        let attackRoll = d20 attackerStatus
                        printfn "Roll: %d" attackRoll
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
                            let dmg = DieRoll.eval (List.append a.Damage (critBonus a.Damage) |> addSneak)
                            printf "CRITICAL HIT! %s %s %s: " this.Name a.Text target.Name
                            target.TakeDamage dmg Weapon
                        elif attackRoll + a.ToHit >= target.AC then
                            if attackRoll + a.ToHit < (target.AC + target.Prof) && hasTrait target DefensiveDuelist && target.TryReact() then
                                printfn "%s misses %s (parried)" this.Name target.Name
                            else
                                let dmg = DieRoll.eval (a.Damage |> addSneak)
                                printf "Hit! %s %s %s: " this.Name a.Text target.Name
                                target.TakeDamage dmg Weapon
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
                                        let dmg = hitRate * avg att.Damage + critRate * avg (critBonus att.Damage)
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
                        isGrappled <- false
                        isProne <- false
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
let scuzName() =
    sprintf "%s the Death Scuz" undeadNames.[r.Next(undeadNames.Length)]
let earthName() =
    sprintf "%s the Earthling" earthNames.[r.Next(earthNames.Length)]
let banditName() =
    sprintf "%s the Bandit Captain" humanNames.[r.Next(humanNames.Length)]
let ogreName() =
    sprintf "%s the Ogre" giantNames.[r.Next(giantNames.Length)]

let deathScuzz() = Combatant(scuzName(), (20, 15, 19, 15, 10, 18, 170), AC=18, Regen=10, Blindsight=true,
                     Actions = [
//                        Action.Create("Cloudkill", ConcentrationEffect(15, All, [Blinded; Damage(SaveForHalf(Con, 15, DieRoll.Create(5,8), Poison))]), 1)
//                        Action.Create("Fear", ConcentrationEffect(15, EnemyOnly, [Afraid]), 2)
                        Action.Create("Multiattack", Attack [
                                                                Attack.Create "bites" 9 [DieRoll.Create(1, 8, 5); DieRoll.Create(2, 6)]
                                                                Attack.Create "cuts" 9 [DieRoll.Create(2, 6, 5); DieRoll.Create(2, 6)]
                                                                Attack.Create "cuts" 9 [DieRoll.Create(2, 6, 5); DieRoll.Create(2, 6)]
                                                                ])
//                        Action.Create("Fireball", Instant(All, SaveForHalf(Dex, 15, DieRoll.Create(8, 6), Fire)), 2)
                     ])

let earthElemental() = Combatant(earthName(), (20, 8, 20, 5, 10, 5, 126), AC=17,
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

let ogre() = Combatant(ogreName(), (15, 16, 14, 14, 11, 14, 65), AC=15, Traits = [DefensiveDuelist], Prof = +2,
                            Actions = [Action.Create("Club attack",
                                            Attack [
                                                Attack.Create "smashes" 6 [DieRoll.Create(2, 8, 4)]
                                                ])
                            ])

// Rufus was created using PHB standard array (15 14 13 12 10 8), variant human Champion 12, with feats Sharpshooter, Crossbow Expert, and Tough; fighting styles Archery and Defense. Has a +1 Hand Crossbow.
let shooter() = Combatant("Rufus the Archer", (12, 20, 14, 10, 14, 8, 124), AC=19, Traits = [RemarkableAthlete; ImprovedCritical; ActionSurge; AthleticsProficient],
                    Actions = [
                        Action.Create("Crossbow Attack", Attack [
                                                                BestOf (Attack.Create "headshots" 7 [DieRoll.Create(1, 6, 16)], Attack.Create "shoots" 12 [DieRoll.Create(1, 6, 6)])
                                                                BestOf (Attack.Create "headshots" 7 [DieRoll.Create(1, 6, 16)], Attack.Create "shoots" 12 [DieRoll.Create(1, 6, 6)])
                                                                BestOf (Attack.Create "headshots" 7 [DieRoll.Create(1, 6, 16)], Attack.Create "shoots" 12 [DieRoll.Create(1, 6, 6)])
                                                                ])
                        Action.Create("Rapier Attack", Attack [
                                                                BestOf (Grapple, BestOf(ShoveProne, Attack.Create "stabs" 10 [DieRoll.Create(1, 6, 6)]))
                                                                BestOf (Grapple, BestOf(ShoveProne, Attack.Create "stabs" 10 [DieRoll.Create(1, 6, 6)]))
                                                                BestOf (Grapple, BestOf(ShoveProne, Attack.Create "stabs" 10 [DieRoll.Create(1, 6, 6)]))
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
                                                                BestOf (Grapple, BestOf(ShoveProne, Attack.Create "stabs" 10 [DieRoll.Create(1, 8, 8)]))
                                                                BestOf (Grapple, BestOf(ShoveProne, Attack.Create "stabs" 10 [DieRoll.Create(1, 8, 8)]))
                                                                BestOf (Grapple, BestOf(ShoveProne, Attack.Create "stabs" 10 [DieRoll.Create(1, 8, 8)]))
                                                                ])
                    ],
                    BonusActions = [
                        Action.Create("Second Wind", Healing (DieRoll.Create(1, 10, 12)), 1)
                    ])

// D'Artagnan was created using PHB standard array (15 14 13 12 10 8), variant human Champion 5/Swashbuckler 7, with feat Sharpshooter (not used in this combat); and fighting style Archery. Has a +1 Longbow which isn't used in this fight and a +1 rapier.
let swash() = Combatant("D'Artagnan the Swashbuckler", (12, 20, 14, 10, 14, 8, 124), AC=18, Traits = [ImprovedCritical; ActionSurge; AthleticsExpertise; SneakAttack(DieRoll.Create(4, 6)); UncannyDodge],
                    Actions = [
                        Action.Create("Attack", Attack [
                                                                BestOf (Grapple, BestOf(ShoveProne, Attack.Create "stabs" 10 [DieRoll.Create(1, 8, 6)]))
                                                                BestOf (Grapple, BestOf(ShoveProne, Attack.Create "stabs" 10 [DieRoll.Create(1, 8, 6)]))
                                                                ])
                    ],
                    BonusActions = [
                        Action.Create("Second Wind", Healing (DieRoll.Create(1, 10, 5)), 1)
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
    while hasLive side1 && hasLive side2 do
        for c in combatantsInOrder do
            if c.IsAlive then
                match findOpponent c with
                | Some(opponent) -> c.TakeTurn opponent
                | None -> ()
        all |> List.iter printStatus
        all |> List.iter (fun c -> c.newRound())

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

//compare [banditCaptain] [ogre]
compare [ogre] [banditCaptain]
