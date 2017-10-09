# Elmish.Xamarin.Forms
Elmish architecture for Xamarin.Forms

## WARNING
This should very much be considered a draft and at the PoC stage. It is not ready for production yet. The DSL is likely to change substantially. Actual performance at scale is unknown.

## Obvious issues
**No structural equality for events**. When an event handler is present in the tree, it is always considered changed, as there's no structural equality for functions. This means pretty much the whole tree is traversed on every update, even when not strictly necessary. There needs to be a way to determine whether an event handler has changed.

**Hacky setting of events**. Setting an event on the Xanarin.Forms object is extremely hacky as there's no way for a client to clear the collection of subscribed listeners. Plus it is probably pretty slow, but is easily fixed with memoization.

**DSL is clunky.** Specifying callbacks is ugly. Due to many views having properties with the same name, it is often necessary to qualify with the discriminated unions name.

**Writing bindings is repetitive and tedious**. An obvious next step is auto-generating bindings from the Xamarin.Forms assembly. This would facilitate a better DSL and it would be much easier to keep up with Xamarin.Forms features.