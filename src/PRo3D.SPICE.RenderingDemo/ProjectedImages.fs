namespace PRo3D.Core.Gis

open System

module InstrumentImages = 

    open Aardvark.Rendering

    type Extrinsics = 
        | Plain of CameraView

    type Intrinsics = 
        | Plain of Frustum

    type ImageData = 
        | FilePath of string

    type ProjectedImage =
        {
            intrinsics : Intrinsics
            extrinsics : Extrinsics
            image      : Option<ImageData>
        }

    type CameraFocus = 
        | FocusBody of focusedBody : string

    type CameraSource =
        | InBody of body : string

    type Intrinsics with
        member x.ProjTrafo = 
            match x with
            | Intrinsics.Plain frustum -> Frustum.projTrafo frustum


type InstrumentProjection = 
    {
        instrumentReferenceFrame : string
        target : InstrumentImages.CameraFocus
        cameraSource : InstrumentImages.CameraSource
        instrumentName : string
        supportBody : string
        time : DateTime
    }

module InstrumentProjection = 

    module Serialization =

        open FSharp.Json

        let serialize (projection: InstrumentProjection) : string =
            let json = Json.serialize projection
            json

        let deserialize (json: string) : InstrumentProjection =
            let projection = Json.deserialize<InstrumentProjection>(json)
            projection
