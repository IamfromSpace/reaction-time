module Poms exposing (Feeling, Score, SubTotal, TestResult, allFeelings, allScores, insert, memptyMaybe, scoreItem, scoreResult, sequenceMaybe, showFeeling, showScore, subTotal, total)


type Score
    = NotAtAll
    | ALittle
    | Moderately
    | QuiteALot
    | Extremely


showScore : Score -> String
showScore score =
    case score of
        NotAtAll ->
            "Not at All"

        ALittle ->
            "A Little"

        Moderately ->
            "Moderately"

        QuiteALot ->
            "Quite a Lot"

        Extremely ->
            "Extremely"


allScores =
    [ NotAtAll
    , ALittle
    , Moderately
    , QuiteALot
    , Extremely
    ]


type alias SubTotal =
    { tension : Int
    , depression : Int
    , anger : Int
    , fatigue : Int
    , confusion : Int
    , vigor : Int
    }


total : SubTotal -> Int
total { tension, depression, anger, fatigue, confusion, vigor } =
    tension + depression + anger + fatigue + confusion - vigor


type alias TestResult a =
    { tense : a
    , shaky : a
    , onEdge : a
    , panicky : a
    , relaxed : a
    , uneasy : a
    , restless : a
    , nervous : a
    , anxious : a
    , unhappy : a
    , sorryForThingsDone : a
    , sad : a
    , blue : a
    , hopeless : a
    , unworthy : a
    , discouraged : a
    , lonely : a
    , miserable : a
    , gloomy : a
    , desperate : a
    , helpless : a
    , worthless : a
    , terrified : a
    , guilty : a
    , angry : a
    , peeved : a
    , grouchy : a
    , annoyed : a
    , resentful : a
    , bitter : a
    , readyToFight : a
    , rebellious : a
    , deceived : a
    , furious : a
    , badTempered : a
    , wornOut : a
    , listless : a
    , fatigued : a
    , exhausted : a
    , sluggish : a
    , weary : a
    , bushed : a
    , confused : a
    , unableToConcentrate : a
    , muddled : a
    , bewildered : a
    , efficient : a
    , forgetful : a
    , uncertainAboutThings : a
    , lively : a
    , active : a
    , energetic : a
    , cheerful : a
    , alert : a
    , fullOfPep : a
    , carefree : a
    , vigorous : a
    }


memptyMaybe : TestResult (Maybe a)
memptyMaybe =
    { tense = Nothing
    , shaky = Nothing
    , onEdge = Nothing
    , panicky = Nothing
    , relaxed = Nothing
    , uneasy = Nothing
    , restless = Nothing
    , nervous = Nothing
    , anxious = Nothing
    , unhappy = Nothing
    , sorryForThingsDone = Nothing
    , sad = Nothing
    , blue = Nothing
    , hopeless = Nothing
    , unworthy = Nothing
    , discouraged = Nothing
    , lonely = Nothing
    , miserable = Nothing
    , gloomy = Nothing
    , desperate = Nothing
    , helpless = Nothing
    , worthless = Nothing
    , terrified = Nothing
    , guilty = Nothing
    , angry = Nothing
    , peeved = Nothing
    , grouchy = Nothing
    , annoyed = Nothing
    , resentful = Nothing
    , bitter = Nothing
    , readyToFight = Nothing
    , rebellious = Nothing
    , deceived = Nothing
    , furious = Nothing
    , badTempered = Nothing
    , wornOut = Nothing
    , listless = Nothing
    , fatigued = Nothing
    , exhausted = Nothing
    , sluggish = Nothing
    , weary = Nothing
    , bushed = Nothing
    , confused = Nothing
    , unableToConcentrate = Nothing
    , muddled = Nothing
    , bewildered = Nothing
    , efficient = Nothing
    , forgetful = Nothing
    , uncertainAboutThings = Nothing
    , lively = Nothing
    , active = Nothing
    , energetic = Nothing
    , cheerful = Nothing
    , alert = Nothing
    , fullOfPep = Nothing
    , carefree = Nothing
    , vigorous = Nothing
    }


type Feeling
    = Tense
    | Shaky
    | OnEdge
    | Panicky
    | Relaxed
    | Uneasy
    | Restless
    | Nervous
    | Anxious
    | Unhappy
    | SorryForThingsDone
    | Sad
    | Blue
    | Hopeless
    | Unworthy
    | Discouraged
    | Lonely
    | Miserable
    | Gloomy
    | Desperate
    | Helpless
    | Worthless
    | Terrified
    | Guilty
    | Angry
    | Peeved
    | Grouchy
    | Annoyed
    | Resentful
    | Bitter
    | ReadyToFight
    | Rebellious
    | Deceived
    | Furious
    | BadTempered
    | WornOut
    | Listless
    | Fatigued
    | Exhausted
    | Sluggish
    | Weary
    | Bushed
    | Confused
    | UnableToConcentrate
    | Muddled
    | Bewildered
    | Efficient
    | Forgetful
    | UncertainAboutThings
    | Lively
    | Active
    | Energetic
    | Cheerful
    | Alert
    | FullOfPep
    | Carefree
    | Vigorous
    | Friendly
    | ClearHeaded
    | Considerate
    | Sympathetic
    | Helpful
    | GoodNatured
    | Trusting


showFeeling : Feeling -> String
showFeeling feeling =
    case feeling of
        Tense ->
            "Tense"

        Shaky ->
            "Shaky"

        OnEdge ->
            "On Edge"

        Panicky ->
            "Panicky"

        Relaxed ->
            "Relaxed"

        Uneasy ->
            "Uneasy"

        Restless ->
            "Restless"

        Nervous ->
            "Nervous"

        Anxious ->
            "Anxious"

        Unhappy ->
            "Unhappy"

        SorryForThingsDone ->
            "Sorry For Things Done"

        Sad ->
            "Sad"

        Blue ->
            "Blue"

        Hopeless ->
            "Hopeless"

        Unworthy ->
            "Unworthy"

        Discouraged ->
            "Discouraged"

        Lonely ->
            "Lonely"

        Miserable ->
            "Miserable"

        Gloomy ->
            "Gloomy"

        Desperate ->
            "Desperate"

        Helpless ->
            "Helpless"

        Worthless ->
            "Worthless"

        Terrified ->
            "Terrified"

        Guilty ->
            "Guilty"

        Angry ->
            "Angry"

        Peeved ->
            "Peeved"

        Grouchy ->
            "Grouchy"

        Annoyed ->
            "Annoyed"

        Resentful ->
            "Resentful"

        Bitter ->
            "Bitter"

        ReadyToFight ->
            "Ready To Fight"

        Rebellious ->
            "Rebellious"

        Deceived ->
            "Deceived"

        Furious ->
            "Furious"

        BadTempered ->
            "Bad Tempered"

        WornOut ->
            "Worn Out"

        Listless ->
            "Listless"

        Fatigued ->
            "Fatigued"

        Exhausted ->
            "Exhausted"

        Sluggish ->
            "Sluggish"

        Weary ->
            "Weary"

        Bushed ->
            "Bushed"

        Confused ->
            "Confused"

        UnableToConcentrate ->
            "Unable To Concentrate"

        Muddled ->
            "Muddled"

        Bewildered ->
            "Bewildered"

        Efficient ->
            "Efficient"

        Forgetful ->
            "Forgetful"

        UncertainAboutThings ->
            "Uncertain About Things"

        Lively ->
            "Lively"

        Active ->
            "Active"

        Energetic ->
            "Energetic"

        Cheerful ->
            "Cheerful"

        Alert ->
            "Alert"

        FullOfPep ->
            "Full Of Pep"

        Carefree ->
            "Carefree"

        Vigorous ->
            "Vigorous"

        Friendly ->
            "Friendly"

        ClearHeaded ->
            "Clear Headed"

        Considerate ->
            "Considerate"

        Sympathetic ->
            "Sympathetic"

        Helpful ->
            "Helpful"

        GoodNatured ->
            "Good Natured"

        Trusting ->
            "Trusting"


allFeelings : List Feeling
allFeelings =
    [ Tense
    , Shaky
    , OnEdge
    , Panicky
    , Relaxed
    , Uneasy
    , Restless
    , Nervous
    , Anxious
    , Unhappy
    , SorryForThingsDone
    , Sad
    , Blue
    , Hopeless
    , Unworthy
    , Discouraged
    , Lonely
    , Miserable
    , Gloomy
    , Desperate
    , Helpless
    , Worthless
    , Terrified
    , Guilty
    , Angry
    , Peeved
    , Grouchy
    , Annoyed
    , Resentful
    , Bitter
    , ReadyToFight
    , Rebellious
    , Deceived
    , Furious
    , BadTempered
    , WornOut
    , Listless
    , Fatigued
    , Exhausted
    , Sluggish
    , Weary
    , Bushed
    , Confused
    , UnableToConcentrate
    , Muddled
    , Bewildered
    , Efficient
    , Forgetful
    , UncertainAboutThings
    , Lively
    , Active
    , Energetic
    , Cheerful
    , Alert
    , FullOfPep
    , Carefree
    , Vigorous
    , Friendly
    , ClearHeaded
    , Considerate
    , Sympathetic
    , Helpful
    , GoodNatured
    , Trusting
    ]


alter : (a -> a) -> Feeling -> TestResult a -> TestResult a
alter fn feeling result =
    case feeling of
        Tense ->
            { result | tense = fn result.tense }

        Shaky ->
            { result | shaky = fn result.shaky }

        OnEdge ->
            { result | onEdge = fn result.onEdge }

        Panicky ->
            { result | panicky = fn result.panicky }

        Relaxed ->
            { result | relaxed = fn result.relaxed }

        Uneasy ->
            { result | uneasy = fn result.uneasy }

        Restless ->
            { result | restless = fn result.restless }

        Nervous ->
            { result | nervous = fn result.nervous }

        Anxious ->
            { result | anxious = fn result.anxious }

        Unhappy ->
            { result | unhappy = fn result.unhappy }

        SorryForThingsDone ->
            { result | sorryForThingsDone = fn result.sorryForThingsDone }

        Sad ->
            { result | sad = fn result.sad }

        Blue ->
            { result | blue = fn result.blue }

        Hopeless ->
            { result | hopeless = fn result.hopeless }

        Unworthy ->
            { result | unworthy = fn result.unworthy }

        Discouraged ->
            { result | discouraged = fn result.discouraged }

        Lonely ->
            { result | lonely = fn result.lonely }

        Miserable ->
            { result | miserable = fn result.miserable }

        Gloomy ->
            { result | gloomy = fn result.gloomy }

        Desperate ->
            { result | desperate = fn result.desperate }

        Helpless ->
            { result | helpless = fn result.helpless }

        Worthless ->
            { result | worthless = fn result.worthless }

        Terrified ->
            { result | terrified = fn result.terrified }

        Guilty ->
            { result | guilty = fn result.guilty }

        Angry ->
            { result | angry = fn result.angry }

        Peeved ->
            { result | peeved = fn result.peeved }

        Grouchy ->
            { result | grouchy = fn result.grouchy }

        Annoyed ->
            { result | annoyed = fn result.annoyed }

        Resentful ->
            { result | resentful = fn result.resentful }

        Bitter ->
            { result | bitter = fn result.bitter }

        ReadyToFight ->
            { result | readyToFight = fn result.readyToFight }

        Rebellious ->
            { result | rebellious = fn result.rebellious }

        Deceived ->
            { result | deceived = fn result.deceived }

        Furious ->
            { result | furious = fn result.furious }

        BadTempered ->
            { result | badTempered = fn result.badTempered }

        WornOut ->
            { result | wornOut = fn result.wornOut }

        Listless ->
            { result | listless = fn result.listless }

        Fatigued ->
            { result | fatigued = fn result.fatigued }

        Exhausted ->
            { result | exhausted = fn result.exhausted }

        Sluggish ->
            { result | sluggish = fn result.sluggish }

        Weary ->
            { result | weary = fn result.weary }

        Bushed ->
            { result | bushed = fn result.bushed }

        Confused ->
            { result | confused = fn result.confused }

        UnableToConcentrate ->
            { result | unableToConcentrate = fn result.unableToConcentrate }

        Muddled ->
            { result | muddled = fn result.muddled }

        Bewildered ->
            { result | bewildered = fn result.bewildered }

        Efficient ->
            { result | efficient = fn result.efficient }

        Forgetful ->
            { result | forgetful = fn result.forgetful }

        UncertainAboutThings ->
            { result | uncertainAboutThings = fn result.uncertainAboutThings }

        Lively ->
            { result | lively = fn result.lively }

        Active ->
            { result | active = fn result.active }

        Energetic ->
            { result | energetic = fn result.energetic }

        Cheerful ->
            { result | cheerful = fn result.cheerful }

        Alert ->
            { result | alert = fn result.alert }

        FullOfPep ->
            { result | fullOfPep = fn result.fullOfPep }

        Carefree ->
            { result | carefree = fn result.carefree }

        Vigorous ->
            { result | vigorous = fn result.vigorous }

        _ ->
            result


listSequenceMaybe_ : List a -> List (Maybe a) -> Maybe (List a)
listSequenceMaybe_ built list =
    case list of
        (Just h) :: t ->
            listSequenceMaybe_ (h :: built) t

        [] ->
            Just (List.reverse built)

        _ ->
            Nothing


insert : a -> Feeling -> TestResult a -> TestResult a
insert value =
    alter (always value)


listSequenceMaybe : List (Maybe a) -> Maybe (List a)
listSequenceMaybe =
    listSequenceMaybe_ []


sequenceMaybe : TestResult (Maybe a) -> Maybe (TestResult a)
sequenceMaybe { tense, shaky, onEdge, panicky, relaxed, uneasy, restless, nervous, anxious, unhappy, sorryForThingsDone, sad, blue, hopeless, unworthy, discouraged, lonely, miserable, gloomy, desperate, helpless, worthless, terrified, guilty, angry, peeved, grouchy, annoyed, resentful, bitter, readyToFight, rebellious, deceived, furious, badTempered, wornOut, listless, fatigued, exhausted, sluggish, weary, bushed, confused, unableToConcentrate, muddled, bewildered, efficient, forgetful, uncertainAboutThings, lively, active, energetic, cheerful, alert, fullOfPep, carefree, vigorous } =
    let
        list =
            [ tense, shaky, onEdge, panicky, relaxed, uneasy, restless, nervous, anxious, unhappy, sorryForThingsDone, sad, blue, hopeless, unworthy, discouraged, lonely, miserable, gloomy, desperate, helpless, worthless, terrified, guilty, angry, peeved, grouchy, annoyed, resentful, bitter, readyToFight, rebellious, deceived, furious, badTempered, wornOut, listless, fatigued, exhausted, sluggish, weary, bushed, confused, unableToConcentrate, muddled, bewildered, efficient, forgetful, uncertainAboutThings, lively, active, energetic, cheerful, alert, fullOfPep, carefree, vigorous ]
    in
    case listSequenceMaybe list of
        Just [ tense_, shaky_, onEdge_, panicky_, relaxed_, uneasy_, restless_, nervous_, anxious_, unhappy_, sorryForThingsDone_, sad_, blue_, hopeless_, unworthy_, discouraged_, lonely_, miserable_, gloomy_, desperate_, helpless_, worthless_, terrified_, guilty_, angry_, peeved_, grouchy_, annoyed_, resentful_, bitter_, readyToFight_, rebellious_, deceived_, furious_, badTempered_, wornOut_, listless_, fatigued_, exhausted_, sluggish_, weary_, bushed_, confused_, unableToConcentrate_, muddled_, bewildered_, efficient_, forgetful_, uncertainAboutThings_, lively_, active_, energetic_, cheerful_, alert_, fullOfPep_, carefree_, vigorous_ ] ->
            Just
                { tense = tense_
                , shaky = shaky_
                , onEdge = onEdge_
                , panicky = panicky_
                , relaxed = relaxed_
                , uneasy = uneasy_
                , restless = restless_
                , nervous = nervous_
                , anxious = anxious_
                , unhappy = unhappy_
                , sorryForThingsDone = sorryForThingsDone_
                , sad = sad_
                , blue = blue_
                , hopeless = hopeless_
                , unworthy = unworthy_
                , discouraged = discouraged_
                , lonely = lonely_
                , miserable = miserable_
                , gloomy = gloomy_
                , desperate = desperate_
                , helpless = helpless_
                , worthless = worthless_
                , terrified = terrified_
                , guilty = guilty_
                , angry = angry_
                , peeved = peeved_
                , grouchy = grouchy_
                , annoyed = annoyed_
                , resentful = resentful_
                , bitter = bitter_
                , readyToFight = readyToFight_
                , rebellious = rebellious_
                , deceived = deceived_
                , furious = furious_
                , badTempered = badTempered_
                , wornOut = wornOut_
                , listless = listless_
                , fatigued = fatigued_
                , exhausted = exhausted_
                , sluggish = sluggish_
                , weary = weary_
                , bushed = bushed_
                , confused = confused_
                , unableToConcentrate = unableToConcentrate_
                , muddled = muddled_
                , bewildered = bewildered_
                , efficient = efficient_
                , forgetful = forgetful_
                , uncertainAboutThings = uncertainAboutThings_
                , lively = lively_
                , active = active_
                , energetic = energetic_
                , cheerful = cheerful_
                , alert = alert_
                , fullOfPep = fullOfPep_
                , carefree = carefree_
                , vigorous = vigorous_
                }

        _ ->
            Nothing


map : (a -> b) -> TestResult a -> TestResult b
map f { tense, shaky, onEdge, panicky, relaxed, uneasy, restless, nervous, anxious, unhappy, sorryForThingsDone, sad, blue, hopeless, unworthy, discouraged, lonely, miserable, gloomy, desperate, helpless, worthless, terrified, guilty, angry, peeved, grouchy, annoyed, resentful, bitter, readyToFight, rebellious, deceived, furious, badTempered, wornOut, listless, fatigued, exhausted, sluggish, weary, bushed, confused, unableToConcentrate, muddled, bewildered, efficient, forgetful, uncertainAboutThings, lively, active, energetic, cheerful, alert, fullOfPep, carefree, vigorous } =
    { tense = f tense
    , shaky = f shaky
    , onEdge = f onEdge
    , panicky = f panicky
    , relaxed = f relaxed
    , uneasy = f uneasy
    , restless = f restless
    , nervous = f nervous
    , anxious = f anxious
    , unhappy = f unhappy
    , sorryForThingsDone = f sorryForThingsDone
    , sad = f sad
    , blue = f blue
    , hopeless = f hopeless
    , unworthy = f unworthy
    , discouraged = f discouraged
    , lonely = f lonely
    , miserable = f miserable
    , gloomy = f gloomy
    , desperate = f desperate
    , helpless = f helpless
    , worthless = f worthless
    , terrified = f terrified
    , guilty = f guilty
    , angry = f angry
    , peeved = f peeved
    , grouchy = f grouchy
    , annoyed = f annoyed
    , resentful = f resentful
    , bitter = f bitter
    , readyToFight = f readyToFight
    , rebellious = f rebellious
    , deceived = f deceived
    , furious = f furious
    , badTempered = f badTempered
    , wornOut = f wornOut
    , listless = f listless
    , fatigued = f fatigued
    , exhausted = f exhausted
    , sluggish = f sluggish
    , weary = f weary
    , bushed = f bushed
    , confused = f confused
    , unableToConcentrate = f unableToConcentrate
    , muddled = f muddled
    , bewildered = f bewildered
    , efficient = f efficient
    , forgetful = f forgetful
    , uncertainAboutThings = f uncertainAboutThings
    , lively = f lively
    , active = f active
    , energetic = f energetic
    , cheerful = f cheerful
    , alert = f alert
    , fullOfPep = f fullOfPep
    , carefree = f carefree
    , vigorous = f vigorous
    }


scoreItem : Score -> Int
scoreItem x =
    case x of
        NotAtAll ->
            0

        ALittle ->
            1

        Moderately ->
            2

        QuiteALot ->
            3

        Extremely ->
            4


scoreResult : TestResult Score -> TestResult Int
scoreResult =
    map scoreItem


subTotal : TestResult Int -> SubTotal
subTotal { tense, shaky, onEdge, panicky, relaxed, uneasy, restless, nervous, anxious, unhappy, sorryForThingsDone, sad, blue, hopeless, unworthy, discouraged, lonely, miserable, gloomy, desperate, helpless, worthless, terrified, guilty, angry, peeved, grouchy, annoyed, resentful, bitter, readyToFight, rebellious, deceived, furious, badTempered, wornOut, listless, fatigued, exhausted, sluggish, weary, bushed, confused, unableToConcentrate, muddled, bewildered, efficient, forgetful, uncertainAboutThings, lively, active, energetic, cheerful, alert, fullOfPep, carefree, vigorous } =
    { tension = tense + shaky + onEdge + panicky + (4 - relaxed) + uneasy + restless + nervous + anxious
    , depression = unhappy + sorryForThingsDone + sad + blue + hopeless + unworthy + discouraged + lonely + miserable + gloomy + desperate + helpless + worthless + terrified + guilty
    , anger = angry + peeved + grouchy + annoyed + resentful + bitter + readyToFight + rebellious + deceived + furious + badTempered
    , fatigue = wornOut + listless + fatigued + exhausted + sluggish + weary + bushed
    , confusion = confused + unableToConcentrate + muddled + bewildered + (4 - efficient) + forgetful + uncertainAboutThings
    , vigor = lively + active + energetic + cheerful + alert + fullOfPep + carefree + vigorous
    }
