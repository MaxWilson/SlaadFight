let r = System.Random()
let d n dieSize x =
    [for x in 1..n -> r.Next(dieSize) + 1] |> Seq.sum |> (+) x

type DieRoll = { N: int; DieSize: int; Plus: int } with
    static member Create(n, d) = { N = n; DieSize = d; Plus = 0 }
    static member Create(n, d, x) = { N = n; DieSize = d; Plus = x }
type DirectAttack = { Text: string; ToHit: int; Damage: DieRoll list }
type Attack = Direct of DirectAttack | Grapple | ShoveProne | BestOf of Attack * Attack with
    static member Create descriptor t d = Direct { Text = descriptor; ToHit = t; Damage = d }
type AoETarget = All | EnemyOnly
type DamageType = Weapon | Poison | Fire
type SaveAbility = Dex | Con
type Damage = Unavoidable of DieRoll * DamageType | SaveForHalf of SaveAbility * int * DieRoll * DamageType
type Effect = Afraid | Blinded | Damage of Damage
type ActionEffect = Attack of Attack list | Instant of AoETarget * Damage | ConcentrationEffect of int * AoETarget * Effect list | Healing of DieRoll
type Action = { Name : string; Effect: ActionEffect; mutable UsesRemaining: int option } with
    static member Create (name, effect) = { Name = name; Effect = effect; UsesRemaining = None }
    static member Create (name, effect, uses) = { Name = name; Effect = effect; UsesRemaining = Some uses }

type Trait = DefensiveDuelist | MageSlayer | RemarkableAthlete

type Combatant(name, stats) =
    let str, dex, con, int, wis, cha, maxHP = (stats : int * int * int * int * int * int * int)
    let prof = +4 // for this sim, proficiency bonus is +4 for both Slaads and Fighters
    let mutable isProne = false
    let mutable isGrappled = false
    let mutable isAfraid = false
    let mutable hasReaction = true
    let mutable hasAction = true
    let mutable hp = maxHP
    let statBonus x = if x >= 10 then (x - 10) / 2 else -((11 - x) / 2)
    member val Name = name
    member val AC = 10 with get, set
    member val Blindsight = false with get, set
    member val Regen = 0 with get, set
    member val Actions = [] with get, set
    member val BonusActions = [] with get, set
    member val Traits = [] with get, set
    member this.newRound() =
        hasReaction <- true;
        hasAction <- true;
        hp <- min maxHP (hp + this.Regen)
    member this.rollInit() =
        let initBonus = statBonus dex + (if List.contains RemarkableAthlete this.Traits then prof /2 else 0)
        d 1 20 initBonus

let deathScuzz() = Combatant("Black Beastie", (20, 15, 19, 15, 10, 18, 170), AC=18, Regen=10, Blindsight=true,
                     Actions = [
                        Action.Create("Cloudkill", ConcentrationEffect(15, All, [Blinded; Damage(SaveForHalf(Con, 15, DieRoll.Create(5,8), Poison))]), 1)
                        Action.Create("Fear", ConcentrationEffect(15, EnemyOnly, [Afraid]), 2)
                        Action.Create("Multiattack", Attack [
                                                                Attack.Create "bites" 9 [DieRoll.Create(1, 8, 5); DieRoll.Create(2, 6)]
                                                                Attack.Create "cuts" 9 [DieRoll.Create(2, 6, 5); DieRoll.Create(2, 6)]
                                                                Attack.Create "cuts" 9 [DieRoll.Create(2, 6, 5); DieRoll.Create(2, 6)]
                                                                ])
                        Action.Create("Fireball", Instant(All, SaveForHalf(Dex, 15, DieRoll.Create(8, 6), Fire)), 2)
                     ])

// Rufus was created using PHB standard array (15 14 13 12 10 8), variant human Champion 12, with feats Sharpshooter, Crossbow Expert, and Tough; fighting styles Archery and Defense. Has a +1 Hand Crossbow.
let shooter() = Combatant("Rufus", (12, 20, 14, 10, 14, 8, 170), AC=19, Traits = [RemarkableAthlete],
                    Actions = [
                        Action.Create("Attack", Attack [
                                                                BestOf (Attack.Create "headshots" 7 [DieRoll.Create(1, 6, 16)], Attack.Create "shoots" 12 [DieRoll.Create(1, 6, 6)])
                                                                BestOf (Attack.Create "headshots" 7 [DieRoll.Create(1, 6, 16)], Attack.Create "shoots" 12 [DieRoll.Create(1, 6, 6)])
                                                                BestOf (Attack.Create "headshots" 7 [DieRoll.Create(1, 6, 16)], Attack.Create "shoots" 12 [DieRoll.Create(1, 6, 6)])
                                                                ])
                    ],
                    BonusActions = [
                        Action.Create("Second Wind", Healing (DieRoll.Create(1, 10, 12)), 1)
                        Action.Create("Bonus Attack", Attack [BestOf (Attack.Create "headshots" 6 [DieRoll.Create(1, 8, 15)], Attack.Create "shoots" 11 [DieRoll.Create(1, 8, 5)])])
                    ])

// Brutus was created using PHB standard array (15 14 13 12 10 8), variant human Champion 12, with feats Defensive Duelist, Mage Slayer, and Tough; fighting styles Dueling and Defense. Has a +1 Rapier.
let stabber() = Combatant("Brutus", (20, 12, 14, 10, 14, 8, 124), AC=19, Traits = [DefensiveDuelist; MageSlayer; RemarkableAthlete],
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