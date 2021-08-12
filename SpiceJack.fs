open System
open System.Collections.Generic

open SpiceSharp
open SpiceSharp.Components
open SpiceSharp.Simulations
open SpiceSharp.Simulations.IntegrationMethods

open JackSharp
open JackSharp.Ports
open JackSharp.Processing

let bufferCapacity = 1024 * 8
let jackDelay = 1000
let freq = 44100.0

let waveform = Pulse (0.0, 0.5, 1.0 / 5000.0, 0.0, 0.0, 1.0 / 5000.0, 2.0 / 5000.0)

let circuit = Circuit (VoltageSource ("V1", "0", "A", waveform), Resistor ("R1", "A", "B", 10.0), Resistor ("R2", "B", "C", 100.0), Capacitor ("C1", "C", "0", 1.0))

type Quantity =
    | Voltage of string
    | Current of string

let ports = [Voltage "A"; Voltage "C"]

type RealExport = IExport<double>
type ExportQueue(sim, quantity) =
    let queue = new Queue<float32> ()
    let export : RealExport =
        match quantity with
        | Voltage port -> new RealVoltageExport (sim, port) :> RealExport
        | Current device -> new RealCurrentExport (sim, device) :> RealExport

    member this.Monitor = new Object ()
    member val Measured = 0 with get, set

    member this.Write () =
        while this.Measured >= bufferCapacity do ()

        lock this.Monitor (fun () ->
            queue.Enqueue (float32 export.Value)
            this.Measured <- this.Measured + 1)

    member this.Receive () =
        this.Measured <- this.Measured - 1
        queue.Dequeue ()

let processFunc (queues : ExportQueue list) (chunk : ProcessBuffer) =
    let buffCount = chunk.AudioOut.Length
    let buffSize  = chunk.Frames

    List.iteri (fun i (queue : ExportQueue) ->
        while queue.Measured < buffSize do ()

        lock queue.Monitor (fun () ->
            for j in 0 .. buffSize - 1 do
                chunk.AudioOut.[i].Audio.[j] <-
                    queue.Receive ())) queues

let runJack (client : Processor) =
    async {
        do! Async.Sleep jackDelay
        ignore (client.Start ())
        printfn "Jack client started"
    }

let runSpice (sim : Simulation) queues =
    sim.ExportSimulationData.Add (fun ev ->
        List.iter (fun (queue : ExportQueue) ->
            queue.Write ()) queues)

    printfn "Simulation started"
    sim.Run circuit
    printfn "Simulation finished"

[<EntryPoint>]
let main argv =
    let ac = Transient ("AC", FixedEuler (StopTime = +infinity, Step = 1.0 / freq))
    let queues = List.map (fun quantity -> ExportQueue (ac, quantity)) ports

    use client = new Processor ("SPICE#", audioOutPorts = List.length ports)
    client.ProcessFunc <- Action<_> (processFunc queues)

    Async.Start (runJack client)
    runSpice ac queues
    0