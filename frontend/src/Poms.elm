module Poms exposing (Score, SubTotal, TestResult, scoreItem, scoreResult, subTotal, total)


type Score
    = NotAtAll
    | ALittle
    | Moderately
    | QuiteALot
    | Extremely


type alias SubTotal =
    { tension : Int
    , depression : Int
    , anger : Int
    , fatigue : Int
    , confusion : Int
    , vigour : Int
    }


total : SubTotal -> Int
total { tension, depression, anger, fatigue, confusion, vigour } =
    tension + depression + anger + fatigue + confusion - vigour


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


listSequenceMaybe_ : List a -> List (Maybe a) -> Maybe (List a)
listSequenceMaybe_ built list =
    case list of
        (Just h) :: t ->
            listSequenceMaybe_ (h :: built) t

        [] ->
            Just (List.reverse built)

        _ ->
            Nothing


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
    , vigour = lively + active + energetic + cheerful + alert + fullOfPep + carefree + vigorous
    }
