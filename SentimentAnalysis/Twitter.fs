namespace SentimentAnalysis

open Newtonsoft.Json
open Tweetinvi

module Twitter = 

    let getCredentials () = 
        System.IO.Path.GetFullPath (SharedCode.solutionLocation() + @"\Keys.json")
        |> System.IO.File.ReadAllText
        |> (fun x -> Newtonsoft.Json.JsonConvert.DeserializeObject<Core.Authentication.TwitterCredentials> (x))
 
    let setCredentials () =
        let credentials = getCredentials()
        do Auth.SetUserCredentials(credentials.ConsumerKey,credentials.ConsumerSecret,credentials.AccessToken,credentials.AccessTokenSecret)
            |> ignore
        do Auth.ApplicationCredentials <- credentials

    do setCredentials()

    let isGoodTweet (tweet: Tweetinvi.Core.Interfaces.ITweet) =
        (not tweet.IsRetweet) || (tweet.InReplyToUserId <> System.Nullable())

    let getTweets () = 
        let iterations = 1
        let parameters (maxId:int64 option)= 
            let temp = new Tweetinvi.Core.Parameters.TweetSearchParameters("@alaskaair")
            temp.MaximumNumberOfResults <- 3200
            match maxId with 
            | Some i -> temp.MaxId <- i
            | None -> ()
            temp
        Tweetinvi.TweetinviEvents.QueryBeforeExecute.Add( fun a -> a.TwitterQuery.Timeout <- System.TimeSpan.FromSeconds(60.0))    
        try Seq.fold 
                (fun state (i:int) -> 
                    let (maxId,currentTweets) = state
                    let newTweets = Tweetinvi.Search.SearchTweets(parameters maxId)
                    (newTweets |> Seq.map (fun (tweet: Tweetinvi.Core.Interfaces.ITweet) -> tweet.Id) |> Seq.min |> Some, 
                        Seq.append currentTweets newTweets)
                    )
                (None,Seq.empty<Tweetinvi.Core.Interfaces.ITweet>) 
                {1..iterations}
            |> snd
        with
        | exn -> 
            do System.Diagnostics.Debug.WriteLine("Couldn't pull tweets " + (Tweetinvi.ExceptionHandler.GetLastException()).TwitterDescription) 
            Seq.empty<Tweetinvi.Core.Interfaces.ITweet>
        |> Seq.filter isGoodTweet

    let getTweetsWithSentiment () = 
        let goodParameters = 
            
            let temp = new Tweetinvi.Core.Parameters.TweetSearchParameters("%40alaskaair %3A)")
            temp.MaximumNumberOfResults <- 100
            temp
        let badParameters =     
            let temp = new Tweetinvi.Core.Parameters.TweetSearchParameters("%40alaskaair %3A(")
            temp.MaximumNumberOfResults <- 100
            temp
        Tweetinvi.TweetinviEvents.QueryBeforeExecute.Add( fun a -> a.TwitterQuery.Timeout <- System.TimeSpan.FromSeconds(60.0))    
        try 
             Seq.append
                (Tweetinvi.Search.SearchTweets(goodParameters) |> Seq.map (fun x -> (Good,x)))
                (Tweetinvi.Search.SearchTweets(badParameters) |> Seq.map (fun x -> (Bad,x)))
        with
        | exn -> 
            do System.Diagnostics.Debug.WriteLine("Couldn't pull tweets " + (Tweetinvi.ExceptionHandler.GetLastException()).TwitterDescription) 
            Seq.empty<Sentiment*Tweetinvi.Core.Interfaces.ITweet>
        |> Seq.filter (fun (s,x) -> isGoodTweet x)
        |> Seq.map (fun (s,x) -> (x.Id,s))
        |> Map.ofSeq

    let simplifyTweet (tweet: Tweetinvi.Core.Interfaces.ITweet) =
        {Username = tweet.CreatedBy.ScreenName; Text = tweet.Text; Time = tweet.TweetLocalCreationDate; Id = tweet.Id}

    let saveTweetsToFile (filename) (tweets: Tweetinvi.Core.Interfaces.ITweet seq) =
        tweets
        |> Seq.map simplifyTweet
        |> (fun o -> Newtonsoft.Json.JsonConvert.SerializeObject(o,Newtonsoft.Json.Formatting.Indented))
        |> (fun s -> System.IO.File.WriteAllText(filename,s))

    let readTweetsFromFile (filename) =
        filename
        |> System.IO.File.ReadAllText
        |> (fun o -> Newtonsoft.Json.JsonConvert.DeserializeObject<SimpleTweet seq>(o))
