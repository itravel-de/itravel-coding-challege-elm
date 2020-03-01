module Behavior exposing
    ( Behavior
    , BlockResult(..)
    , State
    , Thread
    , WaitResult(..)
    , andThen
    , block
    , blockEvent
    , fire
    , fireAll
    , fireOne
    , initialize
    , log
    , request
    , run
    , wait
    , waitFor
    )


type alias Thread e =
    () -> List (Behavior e)


type alias Threads e =
    List (Thread e)


type Behavior e
    = Request e (Threads e)
    | Wait (e -> WaitResult e)
    | Block (e -> BlockResult)


type WaitResult e
    = Continue (Threads e)
    | Pause


type BlockResult
    = Blocked
    | Free


type State e
    = State (Threads e) (List e)



-- PUBLIC API


initialize : Threads e -> State e
initialize threads =
    State threads []
        |> run


run : State e -> State e
run state =
    case selectEvent state of
        Nothing ->
            state

        Just event ->
            applyEvent event state
                |> run


fire : e -> State e -> State e
fire event (State threads eventLog) =
    State (singleRequestThread event :: threads) eventLog |> run


fireOne : List e -> State e -> State e
fireOne events (State threads eventLog) =
    let
        thread _ =
            events
                |> List.map ((<|) Request >> (|>) [])
    in
    State (thread :: threads) eventLog |> run


fireAll : List e -> State e -> State e
fireAll events (State threads eventLog) =
    let
        newThreads =
            List.map singleRequestThread events
    in
    State (newThreads ++ threads) eventLog |> run


log : State e -> List e
log (State _ eventLog) =
    eventLog


request : e -> Threads e -> Behavior e
request =
    Request


waitFor : e -> Threads e -> Behavior e
waitFor expected next =
    Wait
        (\received ->
            if expected == received then
                Continue next

            else
                Pause
        )


wait : (e -> WaitResult e) -> Behavior e
wait =
    Wait


blockEvent : e -> Behavior e
blockEvent expected =
    Block
        (\received ->
            if expected == received then
                Blocked

            else
                Free
        )


block : (e -> BlockResult) -> Behavior e
block =
    Block


andThen : Behavior e -> Thread e
andThen behavior () =
    [ behavior ]



-- PRIVATE HELPERS


singleRequestThread : e -> Thread e
singleRequestThread event () =
    [ Request event [] ]


applyEvent : e -> State e -> State e
applyEvent event (State threads eventLog) =
    threads
        |> List.concatMap (runBehaviors event)
        |> (\newThreads -> State newThreads (event :: eventLog))


runBehaviors : e -> Thread e -> Threads e
runBehaviors event thread =
    let
        behaviors =
            thread ()

        results =
            List.map (runBehavior event) behaviors

        unchanged =
            List.all ((==) Nothing) results

        newThreads =
            List.concatMap (Maybe.withDefault []) results
    in
    if unchanged then
        [ thread ]

    else
        newThreads


runBehavior : e -> Behavior e -> Maybe (Threads e)
runBehavior event behavior =
    case behavior of
        Request e threads ->
            if event == e then
                Just threads

            else
                Nothing

        Wait fn ->
            case fn event of
                Continue threads ->
                    Just threads

                Pause ->
                    Nothing

        Block _ ->
            Nothing


blockChain : Threads e -> e -> Maybe e
blockChain threads =
    let
        chainable fn =
            Maybe.andThen <|
                \event ->
                    case fn event of
                        Blocked ->
                            Nothing

                        Free ->
                            Just event

        onlyBlocks behavior =
            case behavior of
                Block fn ->
                    Just (chainable fn)

                _ ->
                    Nothing
    in
    threads
        |> List.concatMap invoke
        |> List.filterMap onlyBlocks
        |> List.foldl (<<) Just


selectEvent : State e -> Maybe e
selectEvent (State threads _) =
    let
        blocks =
            blockChain threads

        requestedEvents =
            threads
                |> List.concatMap invoke
                |> List.filterMap onlyRequests

        onlyRequests behavior =
            case behavior of
                Request evt _ ->
                    Just evt

                _ ->
                    Nothing
    in
    requestedEvents
        |> firstWith blocks



-- GENERIC HELPERS


applyLeft : ( a -> b, a ) -> ( a -> b, b )
applyLeft ( func, param ) =
    ( func, func param )


invoke : (() -> a) -> a
invoke fn =
    fn ()


annexLeft : ( a, List b ) -> List ( a, b )
annexLeft ( a, list ) =
    List.map (\b -> ( a, b )) list


maybeIf : a -> Bool -> Maybe a
maybeIf item predicate =
    if predicate then
        Just item

    else
        Nothing


firstWith : (a -> Maybe a) -> List a -> Maybe a
firstWith fn list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            case fn head of
                Nothing ->
                    firstWith fn tail

                Just a ->
                    Just a
