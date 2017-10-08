module Elmish.Xamarin.Forms.VirtualDOM

type Extension<'x> =
    abstract member create: unit -> 'x
    abstract member update: Extension<'x> -> 'x -> 'x option

(* Event signatures *)

[<CustomEquality; NoComparison>]
type UnitEvent =
    | UnitEvent of (unit -> unit)
    override this.Equals(that) = System.Object.ReferenceEquals(this, that)
    override this.GetHashCode() = 0


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
    | VerticalOptions of Xamarin.Forms.LayoutOptions

type LabelAttribute =
    | HorizontalTextAlignment of Xamarin.Forms.TextAlignment
    | Text of string

type TableViewAttribute =
    | HasUnevenRows of bool
    | Intent of Xamarin.Forms.TableIntent
    | RowHeight of int

type View =
    | Label of LabelAttribute list
    | StackLayout of StackLayoutAttribute list * View list
    | TableView of TableViewAttribute list * TableSection list
    | ViewExtension of Extension<Xamarin.Forms.View>


let label attributes = Label attributes

let stackLayout attributes children = StackLayout (attributes, children)

let tableView attributes children = TableView (attributes, children)



(* Pages *)

type ContentPageAttribute =
    | Icon of string
    | Title of string

type Page =
    | ContentPage of ContentPageAttribute list * View
    | MasterDetailPage of Page * Page
    | NavigationPage of Page list


let contentPage attributes content = ContentPage (attributes, content)

let masterDetailPage master detail = MasterDetailPage (master, detail)

let navigationPage content = NavigationPage content


