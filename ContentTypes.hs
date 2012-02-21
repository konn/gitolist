module ContentTypes where
import Yesod.Content
import Prelude

ctypeDic :: [(String, ContentType)]
ctypeDic = [(".xhtml", typeHtml)
           ,(".html", typeHtml)
           ,(".htm", typeHtml)
           ,(".json", typeJson)
           ,(".xml", typeXml)
           ,(".atom", typeAtom)
           ,(".rss", typeRss)
           ,(".jpg", typeJpeg)
           ,(".jpeg", typeJpeg)
           ,(".png", typePng)
           ,(".gif", typeGif)
           ,(".flv", typeFlv)
           ,(".ogv", typeOgv)
           ,(".pdf", typePdf)]

typePdf :: ContentType
typePdf = "applicaion/pdf"

newtype RepPdf = RepPdf Content
instance HasReps RepPdf where
  chooseRep (RepPdf c) _ = return (typePdf, c)

typeZip :: ContentType
typeZip = "applicaion/zip"

newtype RepZip = RepZip Content
instance HasReps RepZip where
  chooseRep (RepZip c) _ = return (typePdf, c)

typeTarball :: ContentType
typeTarball = "application/x-tar-gz"

newtype RepTarball = RepTarball Content
instance HasReps RepTarball where
  chooseRep (RepTarball c) _ = return (typeTarball, c)
