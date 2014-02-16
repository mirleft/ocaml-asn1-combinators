

let extension =
  Asn.(sequence3 (required oid)
                 (optional bool) (* default false *)
                 (required octet_string))

let extensions =
  Asn.(sequence_of extension)


let name =
  let attribute_tv =
    Asn.(sequence2 (required oid) (required any)) in
  let rd_name =
    Asn.(set_of attribute_tv) in
  let rdn_sequence =
    Asn.(sequence_of rd_name) in
  rdn_sequence (* A vacuous choice, actually. *)

let algorithmIdentifier =
  Asn.(sequence2 (required oid)
                 (optional any))

let version =
  Asn.int  (*  v1(0), v2(1), v3(2)  *)

let certificateSerialNumber =
  Asn.int

let time =
  Asn.(choice2 utc_time generalized_time)

let validity =
  Asn.(sequence2 (required time)
                 (required time))

let subjectPublicKeyInfo =
  Asn.(sequence2 (required algorithmIdentifier)
                 (required bit_string))

let uniqueIdentifier =
  Asn.bit_string

let sequence10 a b c d e f g h i j =
  Asn.(
    map (fun (a, (b, (c, (d, (e, (f, (g, (h, (i, j))))))))) ->
          (a, b, c, d, e, f, g, h, i, j))
        (fun (a, b, c, d, e, f, g, h, i, j) ->
          (a, (b, (c, (d, (e, (f, (g, (h, (i, j))))))))))
        (sequence @@ a @ b @ c @ d @ e @ f @ h @ h @ i @ single j)
  )

let tBSCertificate =
  Asn.(sequence10
        (optional @@ explicit 0 version)          (* default v1 *)
        (required @@ certificateSerialNumber)
        (required @@ algorithmIdentifier)
        (required @@ name)
        (required @@ validity)
        (required @@ name)
        (required @@ subjectPublicKeyInfo)
        (optional @@ implicit 1 uniqueIdentifier) (* if present, version is v2 or v3 *)
        (optional @@ implicit 2 uniqueIdentifier) (* same *)
        (optional @@ explicit 3 extensions)       (* v3 if present *)
  )

let certificate =
  Asn.(sequence3 (required tBSCertificate)
                 (required algorithmIdentifier)
                 (required bit_string))

