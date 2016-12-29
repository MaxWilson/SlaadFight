type DieRoll = { N: int; DieSize: int; Plus: int } with
    static member Create n d = { N = n; DieSize = d; Plus = 0 }
    static member Create n d x = { N = n; DieSize = d; Plus = x }
type Attack = { ToHit: int; Damage: DieRoll list }
type AoETarget = All | EnemyOnly
type DamageType = Weapon | Poison | Fire
type Effect = Afraid | Blinded | Damage of DieRoll * DamageType
type ActionEffect = Attack of Attack list | SaveForHalf of int * DieRoll * DamageType | ConcentrationEffect of int * AoETarget * Effect list | Healing of DieRoll
type Action = { Name : string; Effect: ActionEffect; mutable UsesRemaining: int option } with
    static member Create (name, effect) = { Name = name; Effect = effect; UsesRemaining = None }
    static member Create (name, effect, uses) = { Name = name; Effect = effect; UsesRemaining = Some uses }

type Combatant(name, stats) =
    let mutable isProne = false
    let mutable isGrappled = false
    let mutable isAfraid = false
    let str, dex, con, int, wis, cha = (stats : int * int * int * int * int * int)
    member val Name = name
    member val HP = 10 with get, set
    member val Actions = [] with get, set

let deathScuzz = Combatant("Black Beastie", (20, 15, 19, 15, 10, 18), HP=170,
                    Actions = [
                        Action.Create("Fear", ConcentrationEffect(15, EnemyOnly, [Effect.Afraid]))
                        Action.Create("Multiattack", Attack[{ToHit = 9, Damage = [{)
                    ])