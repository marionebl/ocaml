type nucleotide = A | C | G | T

let hamming_distance al bl =
    match Base.List.map2 ~f:(==) al bl with
    | Ok(l) -> Some (Base.List.count ~f:(Base.Bool.equal false) l)
    | _ -> None