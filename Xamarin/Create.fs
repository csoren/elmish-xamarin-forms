module Elmish.Xamarin.Forms.Create

open Elmish.Xamarin.Forms.VirtualDOM
open Elmish.Xamarin.Forms.Update


let cell = function
    | TextCell attributes -> 
        let cell = Xamarin.Forms.TextCell ()
        Attributes.updateAll Attributes.textCell attributes cell
        cell :> Xamarin.Forms.Cell

    | CellExtension ext -> ext.create ()

let section = function
    | TableSection (attributes, cells) ->
        let section = new Xamarin.Forms.TableSection ()
        Attributes.updateAll Attributes.tableSection attributes section

        section.Add (cells |> Seq.map cell)
        section

let tableRoot sections =
    let root = new Xamarin.Forms.TableRoot()
    root.Add (sections |> Seq.map section)
    root

let rec view = function
    | Label attributes ->
        let label = new Xamarin.Forms.Label ()
        Attributes.updateAll Attributes.label attributes label

        label :> Xamarin.Forms.View

    | StackLayout (attributes, children) ->
        let stack = new Xamarin.Forms.StackLayout ()
        Attributes.updateAll Attributes.stackLayout attributes stack

        children
        |> Seq.map view
        |> Seq.iter stack.Children.Add

        stack :> Xamarin.Forms.View

    | TableView (attributes, sections) ->
        let root = tableRoot sections
        let tableView = Xamarin.Forms.TableView ()
        Attributes.updateAll Attributes.tableView attributes tableView

        tableView.Root <- root
        tableView :> Xamarin.Forms.View

    | ViewExtension ext ->
        ext.create ()


let rec page = function
    | ContentPage (attributes, content) ->
        let page = Xamarin.Forms.ContentPage(Content = view content)
        Attributes.updateAll Attributes.contentPage attributes page
        page :> Xamarin.Forms.Page

    | MasterDetailPage (attributes, master, detail) ->
        let page = Xamarin.Forms.MasterDetailPage(Master = page master, Detail = page detail)
        Attributes.updateAll Attributes.masterDetailPage attributes page
        page :> Xamarin.Forms.Page

    | NavigationPage content ->
        let navigationPage = new Xamarin.Forms.NavigationPage()
        content
        |> Seq.map page
        |> Seq.rev
        |> Seq.iter (fun p -> navigationPage.PushAsync(p) |> ignore)

        navigationPage :> Xamarin.Forms.Page

