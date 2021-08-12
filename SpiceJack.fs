open System
open System.Collections.Concurrent

open SpiceSharp
open SpiceSharp.Components
open SpiceSharp.Simulations
open SpiceSharp.Simulations.IntegrationMethods

open JackSharp
open JackSharp.Ports
open JackSharp.Processing

let freq = 44100.0

let waveform = Pulse (0.0, 0.5, 1.0 / 5000.0, 0.0, 0.0, 1.0 / 5000.0, 2.0 / 5000.0)
let circuit = Circuit (VoltageSource ("V1", "0", "in", waveform), Capacitor ("C1", "in", "out", 1.0), Resistor ("R1", "out", "0", 2.0e4))

type Quantity =
    | Voltage of string
    | Current of string

let ports = [Voltage "0"; Voltage "out"]

type RealExport = IExport<double>
type ExportQueue(sim, quantity) =
    let queue = new ConcurrentQueue<float32> ()
    let export : RealExport =
        match quantity with
        | Voltage port -> new RealVoltageExport (sim, port) :> RealExport
        | Current device -> new RealCurrentExport (sim, device) :> RealExport

    member val Measured = 0 with get, set

    member this.Write () =
        queue.Enqueue (float32 export.Value)
        this.Measured <- this.Measured + 1

    member this.Receive () =
        let b, v = queue.TryDequeue ()
        this.Measured <- this.Measured - 1
        v

let processFunc (queues : ExportQueue list) (chunk : ProcessBuffer) =
    let buffCount = chunk.AudioOut.Length
    let buffSize  = chunk.Frames

    List.iteri (fun i (queue : ExportQueue) ->
        while queue.Measured < buffSize do ()

        for j in 0 .. buffSize - 1 do
            chunk.AudioOut.[i].Audio.[j] <-
                queue.Receive ()) queues

let runJack (client : Processor) =
    async {
        do! Async.Sleep 1000
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