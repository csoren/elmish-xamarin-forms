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

type Cell =
    | CellExtension of (Extension<Xamarin.Forms.Cell>)


(* Tables *)

type TableSectionAttribute =
    | Title of string

type TableSection =
    | TableSection of TableSectionAttribute list * Cell list

let tableSection attributes children = TableSection(attributes, children)


(* Views *)

type StackLayoutAttribute =
    | VerticalOptions of Xamarin.Forms.LayoutOptions

type LabelAttribute =
    | HorizontalTextAlignment of Xamarin.Forms.TextAlignment
    | Text of string

type View =
    | StackLayout of StackLayoutAttribute list * View list
    | Label of LabelAttribute list
    | ViewExtension of Extension<Xamarin.Forms.View>


let stackLayout attributes children = StackLayout(attributes, children)

let label attributes = Label(attributes)


(* Pages *)

type ContentPageAttribute =
    | Icon of string
    | Title of string

type Page =
    | ContentPage of ContentPageAttribute list * View
    | MasterDetailPage of Page * Page
    | NavigationPage of Page list


let contentPage attributes content = ContentPage(attributes, content)

let masterDetailPage master detail = MasterDetailPage(master, detail)

let navigationPage content = NavigationPage(content)


