open System
open System.Collections.Generic

open SpiceSharp
open SpiceSharp.Components
open SpiceSharp.Simulations
open SpiceSharp.Simulations.IntegrationMethods

open JackSharp
open JackSharp.Ports
open JackSharp.Processing

let bufferCapacity = 4096 * 8
let jackDelay = 1000
let freq = 44100.0

//let waveform = Pulse (0.0, 0.5, 1.0 / 5000.0, 0.0, 0.0, 1.0 / 5000.0, 2.0 / 5000.0)
let waveform = Sine (0.0, 0.5, 440.0)

let circuit = Circuit (VoltageSource ("V1", "0", "A", waveform), Capacitor ("C1", "A", "B", 1.0), VoltageSource ("I1", "B", "0", 0.0))

type Quantity =
    | Voltage of string
    | Current of string

let ports = [Voltage "A"; Current "I1"]

type RealExport = IExport<double>
type ExportQueue(sim, quantity) =
    let queue = new Queue<float32> ()
    let export : RealExport =
        match quantity with
        | Voltage port -> new RealVoltageExport (sim, port) :> RealExport
        | Current device -> new RealCurrentExport (sim, device) :> RealExport

    let monitor = new Object ()

    member val Measured = 0 with get, set

    member this.Write () =
        while this.Measured >= bufferCapacity do ()

        lock monitor (fun () ->
            queue.Enqueue (float32 export.Value)
            this.Measured <- this.Measured + 1)

    member private this.Get () =
        this.Measured <- this.Measured - 1
        queue.Dequeue ()

    member this.Receive callback =
        lock monitor (fun () -> callback this.Get)

let processFunc (queues : ExportQueue list) (chunk : ProcessBuffer) =
    let buffCount = chunk.AudioOut.Length
    let buffSize  = chunk.Frames

    List.iteri (fun i (queue : ExportQueue) ->
        while queue.Measured < buffSize do ()

        queue.Receive (fun recv ->
            for j in 0 .. buffSize - 1 do
                chunk.AudioOut.[i].Audio.[j] <-
                    recv ())) queues

let runJack (client : Processor) =
    async {
        do! Async.Sleep jackDelay
        ignore (client.Start ())
        printfn "Jack client started"
    }

let runSpice (sim : Simulation) queues =
    async {
        sim.ExportSimulationData.Add (fun ev ->
            List.iter (fun (queue : ExportQueue) ->
                queue.Write ()) queues)

        printfn "Simulation started"
        sim.Run circuit
        printfn "Simulation finished"
    }

[<EntryPoint>]
let main argv =
    let ac = Transient ("AC", FixedEuler (StopTime = +infinity, Step = 1.0 / freq))
    let queues = List.map (fun quantity -> ExportQueue (ac, quantity)) ports

    use client = new Processor ("SPICE#", audioOutPorts = List.length ports)
    client.ProcessFunc <- Action<_> (processFunc queues)
    client.Error.Add (fun ev -> printfn "%s" ev.Error)

    Async.Parallel [runJack client; runSpice ac queues]
    |> Async.Ignore
    |> Async.RunSynchronously

    0