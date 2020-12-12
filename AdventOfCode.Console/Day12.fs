module Day12

open Common
open Serilog
open System.Collections.Generic

type CompassDirection = East | West | North | South
type RelativeDirection = Left | Right
type Angle = D180 | D90 | D270

type Action =
    | MoveAction of direction: CompassDirection * value: int
    | MoveForwardAction of value: int
    | TurnAction of direction: RelativeDirection * value: Angle

type State = {
    direction: CompassDirection;
    x: int;
    y: int;
}

type WaypointState = {
    ship_x: int;
    ship_y: int;
    waypoint_x: int;
    waypoint_y: int;
}

let run (lines: IEnumerable<string>) =
    Log.Information $"Executing Day 12"

    let parse_line (line: string) =
        match (line.[0], (int (line.Substring(1)))) with
            | ('R', 90) -> TurnAction(Right, D90)
            | ('R', 180) -> TurnAction(Right, D180)
            | ('R', 270) -> TurnAction(Right, D270)
            | ('L', 90) -> TurnAction(Left, D90)
            | ('L', 180) -> TurnAction(Left, D180)
            | ('L', 270) -> TurnAction(Left, D270)
            | ('N', x) -> MoveAction(North, x)
            | ('E', x) -> MoveAction(East, x)
            | ('S', x) -> MoveAction(South, x)
            | ('W', x) -> MoveAction(West, x)
            | ('F', x) -> MoveForwardAction(x)
            | _ -> raise (ParseException $"Invalid input string {line}")

    let rec step_program state (program: Action[]) pc =
        if pc = program.Length then
            state
        else
            let new_state = match program.[pc] with
                            | MoveAction(North, value) -> { state with y = state.y - value}
                            | MoveAction(South, value) -> { state with y = state.y + value}
                            | MoveAction(East, value) -> { state with x = state.x + value}
                            | MoveAction(West, value) -> { state with x = state.x - value}
                            | TurnAction(direction, value) -> { state with direction = match (direction, value, state.direction) with
                                                                                        | (Left, D90, East) | (Right, D270, East) -> North
                                                                                        | (Left, D90, North) | (Right, D270, North) -> West
                                                                                        | (Left, D90, West)  | (Right, D270, West) -> South
                                                                                        | (Left, D90, South)  | (Right, D270, South) -> East
                                                                                        | (_, D180, East) -> West
                                                                                        | (_, D180, North) -> South
                                                                                        | (_, D180, West) -> East
                                                                                        | (_, D180, South) -> North
                                                                                        | (Left, D270, East) | (Right, D90, East) -> South
                                                                                        | (Left, D270, South) | (Right, D90, South) -> West
                                                                                        | (Left, D270, West) | (Right, D90, West) -> North
                                                                                        | (Left, D270, North) | (Right, D90, North) -> East }
                            | MoveForwardAction(value) -> match state.direction with
                                                            | North -> { state with y = state.y - value }
                                                            | South -> { state with y = state.y + value }
                                                            | East -> { state with x = state.x + value }
                                                            | West -> { state with x = state.x - value }

            step_program new_state program (pc + 1)

    let rec step_waypoint_program (state: WaypointState) (program: Action[]) (pc: int) =
        if pc = program.Length then
            state
        else
            let new_state = match program.[pc] with
                            | MoveAction(North, value) -> { state with waypoint_y = state.waypoint_y - value}
                            | MoveAction(South, value) -> { state with waypoint_y = state.waypoint_y + value}
                            | MoveAction(East, value) -> { state with waypoint_x = state.waypoint_x + value}
                            | MoveAction(West, value) -> { state with waypoint_x = state.waypoint_x - value}
                            | TurnAction(_, D180) -> { state with waypoint_x = state.waypoint_x * -1; waypoint_y = state.waypoint_y * -1 }
                            | TurnAction(Right, D90) | TurnAction(Left, D270) -> { state with waypoint_x = -1 * state.waypoint_y; waypoint_y = state.waypoint_x }
                            | TurnAction(Right, D270) | TurnAction(Left, D90) -> { state with waypoint_x = state.waypoint_y; waypoint_y = state.waypoint_x * -1 }
                            | MoveForwardAction(value) -> { state with ship_x = state.ship_x + value * state.waypoint_x; ship_y = state.ship_y + value * state.waypoint_y }
            step_waypoint_program new_state program (pc + 1)


    let program = lines |> Seq.map parse_line |> Seq.toArray
    let initial_state = { direction = East; x = 0; y = 0; }
    let initial_waypoint_state = { waypoint_y = -1; waypoint_x = 10; ship_x = 0; ship_y = 0; }

    let final_state = step_program initial_state program 0
    let final_waypoint_state = step_waypoint_program initial_waypoint_state program 0

    Log.Information $"Final state stage 1 ({final_state.x},{final_state.y}) = {final_state.x + final_state.y}"
    Log.Information $"Final state stage 2 ({final_waypoint_state.ship_x},{final_waypoint_state.ship_y}) = {final_waypoint_state.ship_x + final_waypoint_state.ship_y}"

    0