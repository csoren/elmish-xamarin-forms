module Elmish.Xamarin.Forms.Update

open Elmish.Extensions
open Elmish.Xamarin.Forms.VirtualDOM

let updateChildren createFn updateFn viewChildren lastViewChildren (collection : System.Collections.Generic.IList<'a>) =
    let viewsToUpdate =
        Seq.zip3 viewChildren lastViewChildren collection
        |> Seq.mapi (fun i t -> i, (t |||> updateFn))
        |> Seq.bindOption (fun (i, optView) -> optView |> Option.map (fun view -> i, view))
        |> Seq.toArray // make collection eager

    viewsToUpdate |> Seq.iter (fun (i, view) -> collection.[i] <- view)

    let totalViews = viewChildren |> Seq.length
    if totalViews > collection.Count then
        viewChildren
        |> Seq.skip collection.Count
        |> Seq.iter (createFn >> collection.Add)
    else if totalViews < collection.Count then
        for i in collection.Count - 1 .. -1 .. totalViews  do
            collection.RemoveAt i

let rec cell newView lastView (root : Xamarin.Forms.Cell) : Xamarin.Forms.Cell option =
    if newView = lastView then
        None
    else
        match newView, lastView, root with
        | TextCell viewAttributes, TextCell lastViewAttributes, (:? Xamarin.Forms.TextCell as viewRoot) ->
            Attributes.setUpdated Attributes.textCell viewAttributes lastViewAttributes viewRoot
            None
        | CellExtension ext, CellExtension lastExt, _ ->
            ext.update lastExt root

and section newView lastView (viewRoot : Xamarin.Forms.TableSection) : Xamarin.Forms.TableSection option =
    if newView = lastView then
        None
    else
        match newView, lastView with
        | TableSection (viewAttributes, viewChildren), TableSection (lastViewAttributes, lastViewChildren) ->
            updateChildren Create.cell cell viewChildren lastViewChildren viewRoot
            Attributes.setUpdated Attributes.tableSection viewAttributes lastViewAttributes viewRoot
            None

and view newView lastView (root : Xamarin.Forms.View) : Xamarin.Forms.View option =
    if newView = lastView then
        None
    else
        match newView, lastView, root with
        | Button viewAttributes, Button lastViewAttributes, (:? Xamarin.Forms.Button as viewRoot) ->
            Attributes.setUpdated Attributes.button viewAttributes lastViewAttributes viewRoot
            None

        | Entry viewAttributes, Entry lastViewAttributes, (:? Xamarin.Forms.Entry as viewRoot) ->
            Attributes.setUpdated Attributes.entry viewAttributes lastViewAttributes viewRoot
            None

        | Label viewAttributes, Label lastViewAttributes, (:? Xamarin.Forms.Label as viewRoot) ->
            Attributes.setUpdated Attributes.label viewAttributes lastViewAttributes viewRoot
            None

        | StackLayout (viewAttributes, viewChildren), StackLayout (lastViewAttributes, lastViewChildren), (:? Xamarin.Forms.StackLayout as viewRoot) ->
            updateChildren Create.view view viewChildren lastViewChildren viewRoot.Children
            Attributes.setUpdated Attributes.stackLayout viewAttributes lastViewAttributes viewRoot
            None
        
        | TableView (viewAttributes, viewSections), TableView (lastViewAttributes, lastViewSections), (:? Xamarin.Forms.TableView as viewRoot) ->
            updateChildren Create.section section viewSections lastViewSections viewRoot.Root
            Attributes.setUpdated Attributes.tableView viewAttributes lastViewAttributes viewRoot
            None

        | ViewExtension viewExtension, ViewExtension lastViewExtension, _ ->
            viewExtension.update lastViewExtension root

        | _ ->
            Create.view newView |> Some


let rec page newView lastView (root : Xamarin.Forms.Page) : Xamarin.Forms.Page option =
    if newView = lastView then
        None
    else
        match newView, lastView, root with
        | NavigationPage viewPages, NavigationPage lastViewPages, (:? Xamarin.Forms.NavigationPage as viewRoot) ->
            let stack = viewRoot.Navigation.NavigationStack |> Seq.rev |> Seq.toArray
            let updatedPages = Seq.zip3 viewPages lastViewPages stack |> Seq.map (fun t -> t |||> page) |> Seq.toArray
            if updatedPages |> Seq.exists (fun v -> v <> None) then
                viewRoot.PopToRootAsync(false) |> ignore
                Seq.zip stack updatedPages 
                |> Seq.map (fun (v, vopt) -> defaultArg vopt v)
                |> Seq.rev
                |> Seq.iter (fun v -> viewRoot.PushAsync(v) |> ignore)
            None

        | MasterDetailPage (viewAttributes, viewMaster, viewDetail), MasterDetailPage (lastViewAttributes, lastViewMaster, lastViewDetail), (:? Xamarin.Forms.MasterDetailPage as viewRoot) ->
            Attributes.setUpdated Attributes.masterDetailPage viewAttributes lastViewAttributes viewRoot

            page viewMaster lastViewMaster viewRoot.Master
            |> Option.iter (fun page -> viewRoot.Master <- page)

            page viewDetail lastViewDetail viewRoot.Detail
            |> Option.iter (fun page -> viewRoot.Detail <- page)

            None

        | ContentPage (viewAttributes, viewContent), ContentPage (lastViewAttributes, lastViewContent), (:? Xamarin.Forms.ContentPage as viewRoot) ->
            view viewContent lastViewContent viewRoot.Content
            |> Option.iter (fun view -> viewRoot.Content <- view)

            Attributes.setUpdated Attributes.contentPage viewAttributes lastViewAttributes viewRoot
            None
