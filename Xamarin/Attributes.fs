module Elmish.Xamarin.Forms.Attributes

open VirtualDOM

let loadImage name =
    Xamarin.Forms.ImageSource.FromFile name :?> Xamarin.Forms.FileImageSource

let contentPage (view : Xamarin.Forms.ContentPage) = function
    | Icon name ->
        view.Icon <- loadImage name
    | Title title ->
        view.Title <- title

let masterDetailPage (view : Xamarin.Forms.MasterDetailPage) = function
    | IsGestureEnabled isGestureEnabled ->
        view.IsGestureEnabled <- isGestureEnabled
    | IsPresented isPresented ->
        view.IsPresented <- isPresented
    | MasterBehavior masterBehavior ->
        view.MasterBehavior <- masterBehavior
    | OnIsPresentedChanged (BoolEvent.Event boolEvent) ->
        Event.clear "IsPresentedChanged" view
        view.IsPresentedChanged.Add (fun _ -> boolEvent view.IsPresented)

let stackLayout (view : Xamarin.Forms.StackLayout) = function
    | Orientation orientation ->
        view.Orientation <- orientation
    | Spacing spacing ->
        view.Spacing <- spacing
    | StackLayoutAttribute.HorizontalOptions options ->
        view.HorizontalOptions <- options
    | StackLayoutAttribute.VerticalOptions options ->
        view.VerticalOptions <- options

let label (view : Xamarin.Forms.Label) = function
    | LabelAttribute.HorizontalOptions options ->
        view.HorizontalOptions <- options
    | LabelAttribute.VerticalOptions options ->
        view.VerticalOptions <- options
    | LabelAttribute.HorizontalTextAlignment align ->
        view.HorizontalTextAlignment <- align
    | LabelAttribute.Text text ->
        view.Text <- text

let button (view : Xamarin.Forms.Button) = function
    | ButtonAttribute.HorizontalOptions options ->
        view.HorizontalOptions <- options
    | ButtonAttribute.VerticalOptions options ->
        view.VerticalOptions <- options
    | BorderColor color ->
        view.BorderColor <- color
    | BorderRadius radius ->
        view.BorderRadius <- radius
    | BorderWidth width ->
        view.BorderWidth <- width
    | ButtonAttribute.FontAttributes attributes ->
        view.FontAttributes <- attributes
    | ButtonAttribute.FontFamily family ->
        view.FontFamily <- family
    | ButtonAttribute.FontSize size ->
        view.FontSize <- size
    | Image name -> 
        view.Image <- loadImage name
    | ButtonAttribute.Text text ->
        view.Text <- text
    | ButtonAttribute.TextColor color ->
        view.TextColor <- color
    | OnClicked (UnitEvent.Event fn) ->
        Event.clear "Clicked" view
        view.Clicked.Add (fun _ -> fn ())

let entry (view : Xamarin.Forms.Entry) = function
    | EntryAttribute.HorizontalOptions options ->
        view.HorizontalOptions <- options
    | EntryAttribute.Margin thickness ->
        view.Margin <- thickness
    | EntryAttribute.VerticalOptions options ->
        view.VerticalOptions <- options
    | FontAttributes fontAttributes ->
        view.FontAttributes <- fontAttributes
    | FontFamily family ->
        view.FontFamily <- family
    | FontSize size ->
        view.FontSize <- size
    | HorizontalTextAlignment textAlignment ->
        view.HorizontalTextAlignment <- textAlignment
    | IsPassword isPassword ->
        view.IsPassword <- isPassword
    | Placeholder placeholder ->
        view.Placeholder <- placeholder
    | PlaceholderColor color ->
        view.PlaceholderColor <- color
    | Text text ->
        view.Text <- text
    | TextColor color ->
        view.TextColor <- color
    | OnCompleted (UnitEvent.Event fn) ->
        Event.clear "Completed" view
        view.Completed.Add (ignore >> fn)
    | OnTextChanged (StringEvent.Event fn) ->
        Event.clear "TextChanged" view
        view.TextChanged.Add (fun ev -> fn ev.NewTextValue)

let tableSection (view : Xamarin.Forms.TableSection) = function
    | TableSectionAttribute.Title title ->
        view.Title <- title

let tableView (view : Xamarin.Forms.TableView) = function
    | TableViewAttribute.HasUnevenRows hasUnevenRows ->
        view.HasUnevenRows <- hasUnevenRows
    | Intent intent ->
        view.Intent <- intent
    | RowHeight rowHeight ->
        view.RowHeight <- rowHeight

let textCell (view : Xamarin.Forms.TextCell) = function
    | TextCellAttribute.Text text ->
        view.Text <- text
    | TextCellAttribute.TextColor textColor ->
        view.TextColor <- textColor
    | Detail detail ->
        view.Detail <- detail
    | DetailColor detailColor ->
        view.DetailColor <- detailColor
    | OnTapped (UnitEvent.Event event) ->
        Event.clear "Tapped" view
        view.Tapped.Add (ignore >> event)

let updateAll updateFn attributes view =
    attributes |> Seq.iter (updateFn view)

let setUpdated updateFn viewAttributes lastViewAttributes viewRoot =
    let updatedAttributes =
        Seq.zip viewAttributes lastViewAttributes
        |> Seq.filter (fun (v, l) -> v <> l)
        |> Seq.map fst
    updateAll updateFn updatedAttributes viewRoot


