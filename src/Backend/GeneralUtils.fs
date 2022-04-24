module Backend.GeneralUtils

let underLine (s: string) =
    [ for _ in s do yield "-" ]
    |> String.concat ""