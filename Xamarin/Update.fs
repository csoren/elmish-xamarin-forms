module Elmish.Xamarin.Forms.Update

open Elmish.Extensions
open Elmish.Xamarin.Forms.VirtualDOM

module Attributes =
    let contentPage (view : Xamarin.Forms.ContentPage) = function
        | Icon name ->
            view.Icon <- Xamarin.Forms.ImageSource.FromFile name :?> Xamarin.Forms.FileImageSource
        | Title title ->
            view.Title <- title

    let stackLayout (view : Xamarin.Forms.StackLayout) = function
        | VerticalOptions options ->
            view.VerticalOptions <- options

    let label (view : Xamarin.Forms.Label) = function
        | HorizontalTextAlignment align ->
            view.HorizontalTextAlignment <- align
        | Text text ->
            view.Text <- text

    let tableSection (view : Xamarin.Forms.TableSection) = function
        | TableSectionAttribute.Title title ->
            view.Title <- title

    let updateAll updateFn attributes view =
        attributes |> Seq.iter (updateFn view)

    let setUpdated updateFn viewAttributes lastViewAttributes viewRoot =
        let updatedAttributes =
            Seq.zip viewAttributes lastViewAttributes
            |> Seq.filter (fun (v, l) -> v <> l)
            |> Seq.map fst
        updateAll updateFn updatedAttributes viewRoot


module Views =


    let updateChildren updateFn viewChildren lastViewChildren (collection : System.Collections.Generic.IList<'a>) =
        Seq.zip3 viewChildren lastViewChildren collection
        |> Seq.mapi (fun i t -> i, (t |||> updateFn))
        |> Seq.bindOption (fun (i, optView) -> optView |> Option.map (fun view -> i, view))
        |> Seq.iter (fun (i, view) -> collection.[i] <- view)


    let rec updateCell view lastView (root : Xamarin.Forms.Cell) : Xamarin.Forms.Cell option =
        if view = lastView then
            None
        else
            match view, lastView with
            | CellExtension ext, CellExtension lastExt ->
                ext.update lastExt root

    and updateSection view lastView (viewRoot : Xamarin.Forms.TableSection) : Xamarin.Forms.TableSection option =
        if view = lastView then
            None
        else
            match view, lastView with
            | TableSection (viewAttributes, viewChildren), TableSection (lastViewAttributes, lastViewChildren) ->
                updateChildren updateCell viewChildren lastViewChildren viewRoot
                Attributes.setUpdated Attributes.tableSection viewAttributes lastViewAttributes viewRoot
                None

    and updateView view lastView (root : Xamarin.Forms.View) : Xamarin.Forms.View option =
        if view = lastView then
            None
        else
            match view, lastView, root with
            | StackLayout (viewAttributes, viewChildren), StackLayout (lastViewAttributes, lastViewChildren), (:? Xamarin.Forms.StackLayout as viewRoot) ->
                updateChildren updateView viewChildren lastViewChildren viewRoot.Children
                Attributes.setUpdated Attributes.stackLayout viewAttributes lastViewAttributes viewRoot
                None

            | Label viewAttributes, Label lastViewAttributes, (:? Xamarin.Forms.Label as viewRoot) ->
                Attributes.setUpdated Attributes.label viewAttributes lastViewAttributes viewRoot
                None

            | ViewExtension viewExtension, ViewExtension lastViewExtension, _ ->
                viewExtension.update lastViewExtension root


    let rec updatePage view lastView (root : Xamarin.Forms.Page) : Xamarin.Forms.Page option =
        if view = lastView then
            None
        else
            match view, lastView, root with
            | NavigationPage viewPages, NavigationPage lastViewPages, (:? Xamarin.Forms.NavigationPage as viewRoot) ->
                let stack = viewRoot.Navigation.NavigationStack |> Seq.rev
                let updatedPages = Seq.zip3 viewPages lastViewPages stack |> Seq.map (fun t -> t |||> updatePage)
                if updatedPages |> Seq.exists (fun v -> v <> None) then
                    viewRoot.PopToRootAsync(false) |> ignore
                    Seq.zip stack updatedPages 
                    |> Seq.map (fun (v, vopt) -> defaultArg vopt v)
                    |> Seq.rev
                    |> Seq.iter (fun v -> viewRoot.PushAsync(v) |> ignore)
                None

            | MasterDetailPage (viewMaster, viewDetail), MasterDetailPage (lastViewMaster, lastViewDetail), (:? Xamarin.Forms.MasterDetailPage as viewRoot) ->
                updatePage viewMaster lastViewMaster viewRoot.Master
                |> Option.iter (fun page -> viewRoot.Master <- page)

                updatePage viewDetail lastViewDetail viewRoot.Detail
                |> Option.iter (fun page -> viewRoot.Detail <- page)

                None

            | ContentPage (viewAttributes, viewContent), ContentPage (lastViewAttributes, lastViewContent), (:? Xamarin.Forms.ContentPage as viewRoot) ->
                updateView viewContent lastViewContent viewRoot.Content
                |> Option.iter (fun view -> viewRoot.Content <- view)

                Attributes.setUpdated Attributes.contentPage viewAttributes lastViewAttributes viewRoot
                None
