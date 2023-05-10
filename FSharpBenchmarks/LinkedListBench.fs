// This benchmark evaluates the performance difference between Option, ValueOption, and Reference/Pointer
//
// This blog claims that the compiler might optimize options of reference types to references directly:
// https://www.bartoszsypytkowski.com/writing-high-performance-f-code/
//  -> for ValueOptions this would sound reasonable
// 
// Conclusion:
//  - ValueOption<ReferenceType> still stores a "HasValue" separately
//  - Matching and updating ValueOptions does not run as fast as "pure" references
//  - The implementation using references (pointers) with [<AllowNullLiteral>] is somehow slower than ValueOptions -> could not run the DisassemblyDiagnoser to investigate further


//BenchmarkDotNet=v0.13.5, OS=Windows 10 (10.0.19044.2846/21H2/November2021Update)
//Intel Core i7-8700K CPU 3.70GHz (Coffee Lake), 1 CPU, 12 logical and 6 physical cores
//.NET SDK=7.0.203
//  [Host]     : .NET 7.0.5 (7.0.523.17405), X64 RyuJIT AVX2 DEBUG
//  DefaultJob : .NET 7.0.5 (7.0.523.17405), X64 RyuJIT AVX2


//|     Method | Count |     Mean |     Error |    StdDev | Ratio | RatioSD | Code Size |   Gen0 |   Gen1 |   Gen2 | Allocated | Alloc Ratio |
//|----------- |------ |---------:|----------:|----------:|------:|--------:|----------:|-------:|-------:|-------:|----------:|------------:|
//|    OptList |   100 | 9.261 us | 0.4956 us | 1.4612 us |  1.00 |    0.00 |   1,721 B | 0.8850 | 0.8240 | 0.1526 |   5.47 KB |        1.00 |
//| ValOptList |   100 | 6.239 us | 0.1234 us | 0.3377 us |  0.69 |    0.11 |   1,694 B | 0.4425 | 0.1755 | 0.0076 |   2.73 KB |        0.50 |
//|    RefList |   100 | 5.378 us | 0.1075 us | 0.2596 us |  0.59 |    0.09 |   1,603 B | 0.3128 | 0.1068 | 0.0076 |   1.95 KB |        0.36 |


module LinkedListBench
    
    open BenchmarkDotNet.Attributes
    open System

    type ListItemOpt<'a>(value : 'a) =
        
        let mutable next : option<ListItemOpt<'a>> = None
        let mutable prev : option<ListItemOpt<'a>> = None

        member x.Value = value

        member x.Prev
            with get() = prev
            and set p = prev <- p

        member x.Next
            with get() = next
            and set n = next <- n

    type LinkedListOpt<'a>(singleInitial : 'a) =
        
        let init = ListItemOpt(singleInitial)

        let mutable head : option<ListItemOpt<'a>> = Some init
        let mutable tail : option<ListItemOpt<'a>> = Some init

        member x.Head = head
        member x.Tail = tail

        member x.AddBefore(value : 'a, before) =
            
            let item = ListItemOpt(value)

            item.Next <- Some before
            item.Prev <- before.Prev

            match before.Prev with
            | Some p -> p.Next <- Some item
            | None -> head <- Some item

            before.Prev <- Some item

            item

        member x.Remove(item : ListItemOpt<'a>) =
            
            match item.Prev with
            | Some p -> p.Next <- item.Next
            | None -> head <- item.Next

            match item.Next with
            | Some n -> n.Prev <- item.Prev
            | None -> tail <- item.Prev


    type ListItemValOpt<'a>(value : 'a) =
        
        let mutable next : ValueOption<ListItemValOpt<'a>> = ValueNone
        let mutable prev : ValueOption<ListItemValOpt<'a>> = ValueNone

        member x.Value = value

        member x.Prev
            with get() = prev
            and set p = prev <- p

        member x.Next
            with get() = next
            and set n = next <- n

    type LinkedListValOpt<'a>(singleInitial : 'a) =
        
        let init = ListItemValOpt(singleInitial)

        let mutable head : ValueOption<ListItemValOpt<'a>> = ValueSome init
        let mutable tail : ValueOption<ListItemValOpt<'a>> = ValueSome init

        member x.Head = head
        member x.Tail = tail

        member x.AddBefore(value : 'a, before) =
            
            let item = ListItemValOpt(value)

            item.Next <- ValueSome before
            item.Prev <- before.Prev

            match before.Prev with
            | ValueSome p -> p.Next <- ValueSome item
            | ValueNone -> head <- ValueSome item

            before.Prev <- ValueSome item

            item

        member x.Remove(item : ListItemValOpt<'a>) =
            
            match item.Prev with
            | ValueSome p -> p.Next <- item.Next
            | ValueNone -> head <- item.Next

            match item.Next with
            | ValueSome n -> n.Prev <- item.Prev
            | ValueNone -> tail <- item.Prev


    // NOTE: using [<AllowNullLiteral>] is somehow much slower
    type ListItemRef<'a>(value : 'a) =
        
        let mutable next : ListItemRef<'a> = Unchecked.defaultof<_>
        let mutable prev : ListItemRef<'a> = Unchecked.defaultof<_>

        member x.Value = value

        member x.Prev
            with get() = prev
            and set p = prev <- p

        member x.Next
            with get() = next
            and set n = next <- n
    
    type LinkedListRef<'a>(singleInitial : 'a) =
        
        let init = ListItemRef(singleInitial)

        let mutable head : ListItemRef<'a> = init
        let mutable tail : ListItemRef<'a> = init

        member x.Head = head
        member x.Tail = tail

        member x.AddBefore(value : 'a, before) =
            
            let item = ListItemRef(value)

            item.Next <- before
            item.Prev <- before.Prev

            let p = before.Prev
            if Object.ReferenceEquals(p, null) then
                head <- item
            else
                p.Next <- item

            before.Prev <- item

            item

        member x.Remove(item : ListItemRef<'a>) =
            
            let p = item.Prev
            if Object.ReferenceEquals(p, null) then
                head <- item.Next
            else
                p.Next <- item.Next

            let n = item.Next
            if Object.ReferenceEquals(n, null) then
                tail <- item.Prev
            else
                n.Prev <- item.Prev


    //[<InProcess>] // results in crash with DisassemblyDiagnoser
    [<PlainExporter; MemoryDiagnoser>]
    //[<DisassemblyDiagnoser(5, BenchmarkDotNet.Diagnosers.DisassemblySyntax.Masm, true, true, true, true, true, true)>]
    //[<RyuJitX64Job>]
    type LinkedListBench() =
        let mutable optList = LinkedListOpt<_>(5)
        let mutable optListItems = System.Collections.Generic.List<ListItemOpt<_>>(10000)
        let mutable valOptList = LinkedListValOpt<_>(5)
        let mutable valOptListItems = System.Collections.Generic.List<ListItemValOpt<_>>(10000)
        let mutable refList = LinkedListRef<_>(5)
        let mutable refListItems = System.Collections.Generic.List<ListItemRef<_>>(10000)
        let mutable rnd = Random(1)


        [<DefaultValue; Params(100)>]
        val mutable Count : int


        member x.OptListAdd() =
            let value = rnd.Next()
            let idx = rnd.Next(optListItems.Count)
            let pos = optListItems.[idx]
            optListItems.Add(optList.AddBefore(value, pos))

        member x.OptListRemove() =
            if optListItems.Count > 1 then
                let idx = rnd.Next(optListItems.Count)
                let item = optListItems.[idx]
                optList.Remove(item)

                if idx < (optListItems.Count - 1) then
                    optListItems.[idx] <- optListItems.[optListItems.Count - 1]
                optListItems.RemoveAt(optListItems.Count - 1)


        member x.ValOptListAdd() =
            let value = rnd.Next()
            let idx = rnd.Next(valOptListItems.Count)
            let pos = valOptListItems.[idx]
            valOptListItems.Add(valOptList.AddBefore(value, pos))

        member x.ValOptListRemove() =
            if valOptListItems.Count > 1 then
                let idx = rnd.Next(valOptListItems.Count)
                let item = valOptListItems.[idx]
                valOptList.Remove(item)

                if idx < (valOptListItems.Count - 1) then
                    valOptListItems.[idx] <- valOptListItems.[valOptListItems.Count - 1]
                valOptListItems.RemoveAt(valOptListItems.Count - 1)


        member x.RefListAdd() =
            let value = rnd.Next()
            let idx = rnd.Next(refListItems.Count)
            let pos = refListItems.[idx]
            refListItems.Add(refList.AddBefore(value, pos))

        member x.RefListRemove() =
            if refListItems.Count > 1 then
                let idx = rnd.Next(refListItems.Count)
                let item = refListItems.[idx]
                refList.Remove(item)

                if idx < (refListItems.Count - 1) then
                    refListItems.[idx] <- refListItems.[refListItems.Count - 1]
                refListItems.RemoveAt(refListItems.Count - 1)


        [<GlobalSetup>]
        member x.Init() =
            optListItems.Add(optList.Head.Value)
            valOptListItems.Add(valOptList.Head.Value)
            refListItems.Add(refList.Head)
            for i in 1..100 do
                x.OptListAdd()
                x.ValOptListAdd()
                x.RefListAdd()


        [<Benchmark>]
        member x.OptList() =
            for i in 1..x.Count do
                if optListItems.Count < 10000 && (rnd.Next(2) = 0) then
                    x.OptListAdd()
                else
                    x.OptListRemove()

        [<Benchmark>]
        member x.ValOptList() =
            for i in 1..x.Count do
                if valOptListItems.Count < 10000 && (rnd.Next(2) = 0) then
                    x.ValOptListAdd()
                else
                    x.ValOptListRemove()

        [<Benchmark>]
        member x.RefList() =
            for i in 1..x.Count do
                if refListItems.Count < 10000 && (rnd.Next(2) = 0) then
                    x.RefListAdd()
                else
                    x.RefListRemove()
