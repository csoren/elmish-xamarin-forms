module Elmish.Xamarin.Forms.Application
    
open Elmish.Extensions
open Elmish
open Elmish.Xamarin.Forms.Update


type internal App<'model, 'msg>(program: Program<unit, 'model, 'msg, VirtualDOM.Page>) as this =
    inherit Xamarin.Forms.Application()

    // Requires App to be instantiated on the main thread to work.
    // This is happily the case.
    let mainThread = System.Threading.Thread.CurrentThread

    let isOnMainThread () = System.Object.ReferenceEquals(mainThread, System.Threading.Thread.CurrentThread)

    let mutable optLastView = None

    let program' = { program with setState = this.setState program }

    let updateViews view =
        match optLastView with
        | Some lastView ->
            Update.page view lastView this.MainPage
            |> Option.iter (fun page -> this.MainPage <- page)
        | None ->
            this.MainPage <- Create.page view
        optLastView <- Some view

    let runOnMainThread action =
        if isOnMainThread () then
            action ()
        else
            Xamarin.Forms.Device.BeginInvokeOnMainThread(fun () -> action())

    do Program.run program'

    member this.setState (program: Program<unit, 'model, 'msg, VirtualDOM.Page>) model dispatch =
        let view = program.view model dispatch

        runOnMainThread (fun () -> updateViews view)


let asApplication<'model, 'msg> (program: Program<unit, 'model, 'msg, VirtualDOM.Page>) =
    new App<'model, 'msg>(program) :> Xamarin.Forms.Application


