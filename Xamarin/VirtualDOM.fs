module Elmish.Xamarin.Forms.VirtualDOM

type Extension<'x> =
    abstract member create: unit -> 'x
    abstract member update: Extension<'x> -> 'x -> 'x option

(* Event signatures *)

[<CustomEquality; NoComparison>]
type Event<'t> =
    | Event of ('t -> unit)
    override this.Equals(that) = System.Object.ReferenceEquals(this, that)
    override this.GetHashCode() = 0

type UnitEvent = Event<unit>
type BoolEvent = Event<bool>


(* Cells *)

type TextCellAttribute =
    | Text of string
    | TextColor of Xamarin.Forms.Color
    | Detail of string
    | DetailColor of Xamarin.Forms.Color
    | OnTapped of UnitEvent

type Cell =
    | TextCell of TextCellAttribute list
    | CellExtension of (Extension<Xamarin.Forms.Cell>)

let textCell attributes = TextCell attributes


(* Tables *)

type TableSectionAttribute =
    | Title of string

type TableSection =
    | TableSection of TableSectionAttribute list * Cell list

let tableSection attributes children = TableSection (attributes, children)


(* Views *)

type StackLayoutAttribute =
    | Orientation of Xamarin.Forms.StackOrientation
    | Spacing of float
    | HorizontalOptions of Xamarin.Forms.LayoutOptions
    | VerticalOptions of Xamarin.Forms.LayoutOptions

type LabelAttribute =
    | HorizontalOptions of Xamarin.Forms.LayoutOptions
    | VerticalOptions of Xamarin.Forms.LayoutOptions
    | HorizontalTextAlignment of Xamarin.Forms.TextAlignment
    | Text of string

type TableViewAttribute =
    | HasUnevenRows of bool
    | Intent of Xamarin.Forms.TableIntent
    | RowHeight of int

type ButtonAttribute =
    | HorizontalOptions of Xamarin.Forms.LayoutOptions
    | VerticalOptions of Xamarin.Forms.LayoutOptions
    | BorderColor of Xamarin.Forms.Color
    | BorderRadius of int
    | BorderWidth of float
    | FontAttributes of Xamarin.Forms.FontAttributes
    | FontFamily of string
    | FontSize of float
    | Image of string
    | Text of string
    | TextColor of Xamarin.Forms.Color
    | OnClicked of UnitEvent

type View =
    | Button of ButtonAttribute list
    | Label of LabelAttribute list
    | StackLayout of StackLayoutAttribute list * View list
    | TableView of TableViewAttribute list * TableSection list
    | ViewExtension of Extension<Xamarin.Forms.View>


let label attributes = Label attributes

let stackLayout attributes children = StackLayout (attributes, children)

let tableView attributes children = TableView (attributes, children)

let button attributes = Button attributes


(* Pages *)

type ContentPageAttribute =
    | Icon of string
    | Title of string

type MasterDetailPageAttribute =
    | IsGestureEnabled of bool
    | IsPresented of bool
    | MasterBehavior of Xamarin.Forms.MasterBehavior
    | OnIsPresentedChanged of BoolEvent

type Page =
    | ContentPage of ContentPageAttribute list * View
    | MasterDetailPage of MasterDetailPageAttribute list * Page * Page
    | NavigationPage of Page list


let contentPage attributes content = ContentPage (attributes, content)

let masterDetailPage attributes master detail = MasterDetailPage (attributes, master, detail)

let navigationPage content = NavigationPage content


