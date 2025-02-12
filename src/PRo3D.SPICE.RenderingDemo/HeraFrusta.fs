#if INTERACTIVE
#r "nuget: Aardvark.Base"
#else
namespace PRo3D.SPICE
#endif

open System
open System.IO
open System.Text.Json

open Aardvark.Base

[<AutoOpen>]
module Frusta = 


    type FitsDescription = 
        {
            observationDate : DateTime
            instrument : string
            missionPhase : string
            imageSize : V2i
        }

    let parseFitsDescription (jsonString: string) =
        let jsonDoc = JsonDocument.Parse(jsonString)
        let root = jsonDoc.RootElement
        let heraFits = root.GetProperty("hera_fits")
        
        let image_width = root.GetProperty("image_width").GetInt32()
        let image_height = root.GetProperty("image_height").GetInt32()
        let observationDate = heraFits.GetProperty("DATE-OBS").GetString() |> DateTime.Parse
        let instrument = heraFits.GetProperty("INSTRUME").GetString()
        let missionPhase = heraFits.GetProperty("MISSPHAS").GetString()

        {
            observationDate = observationDate
            instrument = instrument
            missionPhase = missionPhase
            imageSize = V2i(image_width, image_height)
        }

    let parseFitsDescriptionFromFile (path: string) =
        File.ReadAllText path |> parseFitsDescription


    let parseObservations (fitsJsonDir : string) =
        Directory.EnumerateFiles(fitsJsonDir, "*.tif")
        |> Seq.toArray
        |> Array.map (fun tifPath ->
            let fits = parseFitsDescriptionFromFile $"{tifPath}.json"
            let img = tifPath
            (fits, img)
        )

    let test = 
        let testPath = @"C:\pro3ddata\HERA\HERA_Vision_2B-3\TIFFS_EarthMoon\EarthMoon\AFC1\AF1_00BAO3_241011T141702_1A.tif.json"
        testPath |> parseFitsDescriptionFromFile

    let testDir = 
        let testPath = @"C:\pro3ddata\HERA\HERA_Vision_2B-3\TIFFS_EarthMoon\EarthMoon\AFC1\"
        testPath |> parseObservations