namespace SentimentAnalysis


    
type AnalyzedTweet =
    {Tweet: SimpleTweet;
     BagOfWords: Set<string>;
     Sentiment: Sentiment option;
     SentimentValue: float}

type WordParameter =
    {ProbabilityBad: float;
     ProbabilityGood: float}

type ParameterSet =
    {WordParameters: Map<string,WordParameter>;
     ProbabilityGood: float}
module SentimentAnalysis = 
    let removeSpecialCharactersAndFormat (str:string) = 
        let sb = new System.Text.StringBuilder()
        for c in str do
            if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) then
                sb.Append(c) |> ignore
            else ()
        sb.ToString().ToLower()

    let stopWords =
        SharedCode.projectLocation() + "StopWords.json"
        |> System.IO.File.ReadAllText
        |> Newtonsoft.Json.Linq.JObject.Parse
        |> (fun x -> x.["StopWords"])
        |> Seq.map string
        |> Seq.map removeSpecialCharactersAndFormat
        |> Set.ofSeq

    let keywords = 
        seq ["@alaskaair"]
        |> Seq.map removeSpecialCharactersAndFormat
        |> Set.ofSeq

    let initialBadWords =
        seq ["terrible"; "poor"; "bad"]
        |> Seq.map removeSpecialCharactersAndFormat
        |> Set.ofSeq

    let initialGoodWords =
        seq ["great"; "excellent"; "good"]
        |> Seq.map removeSpecialCharactersAndFormat
        |> Set.ofSeq

    let splitSentence (sentence:string) =
        sentence.Split([|' ';'\n';'\t'|])
        |> Set.ofArray
        

    let getBagOfWords (sentence:string) =
        sentence
        |> splitSentence
        |> Set.map removeSpecialCharactersAndFormat
        |> Set.filter (fun word -> not (Set.contains word keywords))
        |> Set.filter (fun word -> not (Set.contains word stopWords))
        |> Set.filter (System.String.IsNullOrEmpty >> not)
        
    let initialSentiment (bag: Set<string>) =
        let badCount =
            bag
            |> Set.intersect initialBadWords
            |> Set.count
        let goodCount =
            bag
            |> Set.intersect initialGoodWords
            |> Set.count
        if badCount > goodCount then
            Some Bad
        else if badCount < goodCount then
            Some Good
        else None

    let scoreToSentiment (probabilityGood) (score:float) =
        if score < probabilityGood - 0.05 then Some Bad
        else if score >= probabilityGood + 0.05 then Some Good
        else None

    let initializeTweets (tweets: SimpleTweet seq) =
        let tweetsWithSentiments =
            tweets
            |> Seq.map (fun tweet -> 
                let bag = getBagOfWords tweet.Text 
                (tweet,bag, initialSentiment bag))
        let probabilityGood =
            let counts = tweetsWithSentiments |> Seq.countBy (fun (t,b,s) -> s) |> Map.ofSeq
            match (Map.tryFind (Some Good) counts,Map.tryFind (Some Bad) counts) with
            | (Some goodCount, Some badCount) -> (float goodCount) / (float (goodCount + badCount))
            | _ -> 0.5
        tweetsWithSentiments
        |> Seq.map (fun (tweet,bagOfWords, sentiment) ->
            let value = match sentiment with
                                | Some Good -> 1.0
                                | Some Bad -> 0.0
                                | None -> probabilityGood
            {Tweet = tweet; BagOfWords = bagOfWords; Sentiment = sentiment; SentimentValue = value})

    let getParametersFromTweets (tweets: AnalyzedTweet seq) =
        let badTweets = tweets
                        |> Seq.filter (fun tweet -> tweet.Sentiment = Some Bad)
                        |> Seq.cache
        let goodTweets = tweets
                        |> Seq.filter (fun tweet -> tweet.Sentiment = Some Good)
                        |> Seq.cache
        let totalBadTweets = Seq.length badTweets
        let totalGoodTweets = Seq.length goodTweets
        let probabilityGood = (float (totalGoodTweets))/(float (totalBadTweets + totalGoodTweets))
        let words = 
            (Seq.append badTweets goodTweets)
            |> Seq.map (fun tweet -> tweet.BagOfWords)
            |> Set.unionMany

        let wordParameters =
            words
            |> Seq.map (fun word ->
                let badCount =
                    badTweets
                    |> Seq.filter (fun tweet -> Set.contains word tweet.BagOfWords)
                    |> Seq.length
                let goodCount =
                    goodTweets
                    |> Seq.filter (fun tweet -> Set.contains word tweet.BagOfWords)
                    |> Seq.length
                (word,badCount,goodCount))
            |> Seq.filter (fun (word,badCount,goodCount) -> badCount + goodCount >= 4) 
            |> Seq.map (fun (word,badCount,goodCount) ->
                (word,{ProbabilityBad = (float (badCount+1))/ (float (totalBadTweets + 10));
                        ProbabilityGood = (float (goodCount+1))/ (float (totalGoodTweets + 10))})
                    )
            |> Seq.filter (fun (word,parameters) -> parameters.ProbabilityBad > 2.0* parameters.ProbabilityGood || parameters.ProbabilityGood > 2.0* parameters.ProbabilityBad )
            |> Map.ofSeq
        {WordParameters = wordParameters; ProbabilityGood = probabilityGood}

    let getScoreFromParameters (parameters: ParameterSet) (bagOfWords: Set<string>) =
        let wordsInTweet = 
            bagOfWords
            |> Set.toSeq
            |> Seq.choose (fun word -> Map.tryFind word parameters.WordParameters)
        let (goodWordScore,badWordScore) =
            wordsInTweet
            |> Seq.fold (fun (goodWordScore,badWordScore) parameter -> (goodWordScore*parameter.ProbabilityGood,badWordScore*parameter.ProbabilityBad)) (1.0,1.0)
        goodWordScore*parameters.ProbabilityGood/(parameters.ProbabilityGood*goodWordScore+(1.0-parameters.ProbabilityGood)*badWordScore)

    let updateTweet (parameters:ParameterSet) (smoothing: float) (tweet: AnalyzedTweet) =
        let newScore = getScoreFromParameters parameters tweet.BagOfWords
        let newSentiment = scoreToSentiment parameters.ProbabilityGood newScore
        {tweet with SentimentValue = smoothing*newScore+(1.0-smoothing)*tweet.SentimentValue; Sentiment = newSentiment}
        
    let emTweets (smoothing: float) (iterations: int) (tweets: SimpleTweet seq) =
        let initialTweets = 
            tweets
            |> initializeTweets
            |> Seq.cache
        Seq.scan 
            (fun state iteration ->
                let tweets = snd state
                let parameters: ParameterSet option = fst state
                let newParameters = getParametersFromTweets tweets
                let newTweets =
                    tweets
                    |> Seq.map (updateTweet newParameters smoothing)
                ((Some newParameters),newTweets)
                )
            (None,initialTweets)
            {1..iterations}
        |> Seq.cache
