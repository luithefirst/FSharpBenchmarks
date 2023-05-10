
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


    [<AllowNullLiteral>] 
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
            if p <> null then
                p.Next <- item
            else
                head <- item

            before.Prev <- item

            item

        member x.Remove(item : ListItemRef<'a>) =
            
            let p = item.Prev
            if p <> null then
                p.Next <- item.Next
            else
                head <- item.Next

            let n = item.Next
            if n <> null then
                n.Prev <- item.Prev
            else
                tail <- item.Prev


    [<InProcess>]
    [<PlainExporter; MemoryDiagnoser>]
    //[<DisassemblyDiagnoser(5, BenchmarkDotNet.Diagnosers.DisassemblySyntax.Intel, false, false, false, false, false, false)>]
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


        [<Benchmark(Baseline = true)>]
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
