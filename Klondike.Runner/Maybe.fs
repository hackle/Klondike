module Maybe

type MaybeBuilder() =
    member this.Return(x) = Some x
    member this.ReturnFrom(x) = x
    member this.Bind(o, f) = Option.bind f o

let maybe = MaybeBuilder()

