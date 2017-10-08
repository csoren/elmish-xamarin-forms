module Elmish.Xamarin.Forms.Create

open Elmish.Xamarin.Forms.VirtualDOM
open Elmish.Xamarin.Forms.Update


let cell = function
    | CellExtension ext -> ext.create ()

let section = function
    | TableSection (attributes, cells) ->
        let section = new Xamarin.Forms.TableSection ()
        Attributes.updateAll Attributes.tableSection attributes section

        cells
        |> Seq.map cell
        |> section.Add
        section

let rec view = function
    | StackLayout (attributes, children) ->
        let stack = new Xamarin.Forms.StackLayout ()
        Attributes.updateAll Attributes.stackLayout attributes stack

        children
        |> Seq.map view
        |> Seq.iter stack.Children.Add

        stack :> Xamarin.Forms.View

    | Label attributes ->
        let label = new Xamarin.Forms.Label ()
        Attributes.updateAll Attributes.label attributes label

        label :> Xamarin.Forms.View

    | ViewExtension ext ->
        ext.create ()


let rec page = function
    | ContentPage (attributes, content) ->
        let page = new Xamarin.Forms.ContentPage(Content = view content)
        Attributes.updateAll Attributes.contentPage attributes page
        page :> Xamarin.Forms.Page

    | MasterDetailPage (master, detail) ->
        new Xamarin.Forms.MasterDetailPage(Master = page master, Detail = page detail)
        :> Xamarin.Forms.Page

    | NavigationPage content ->
        let navigationPage = new Xamarin.Forms.NavigationPage()
        content
        |> Seq.map page
        |> Seq.rev
        |> Seq.iter (fun p -> navigationPage.PushAsync(p) |> ignore)

        navigationPage :> Xamarin.Forms.Page

