module Elmish.Xamarin.Forms.Event

open System.Reflection
open System

// This is a fairly hacky solution to clearing all registered listeners from an event,
// based on this answer on Stack Overflow
// https://stackoverflow.com/a/8108103/619821


// Find events defined as field
let private getFieldEvent eventName (typ : Type) =
    typ.GetField(eventName, BindingFlags.Static ||| BindingFlags.Instance ||| BindingFlags.NonPublic)
    |> Option.ofObj

// Find events defined as property { add; remove; }
let private getPropertyEvent (eventName : string) (typ : Type) =
    typ.GetField("EVENT_" + eventName.ToUpper(), BindingFlags.Static ||| BindingFlags.Instance ||| BindingFlags.NonPublic)
    |> Option.ofObj

let rec private getEventField eventName (typ : Type) =
    match getFieldEvent eventName typ with
    | Some field as optField when field.FieldType = typeof<MulticastDelegate> || field.FieldType.IsSubclassOf(typeof<MulticastDelegate>) ->
        optField
    | _ ->
        match getPropertyEvent eventName typ with
        | Some _ as optField -> optField
        | _ -> getEventField eventName typ.BaseType

let clear eventName object =
    getEventField eventName (object.GetType())
    |> Option.iter (fun fi -> fi.SetValue(object, null))

