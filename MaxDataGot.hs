import Data.Map (Map(..))

data MaxData = MaxData { mdMaterials :: [Material], mdNodes :: Map String Node, mdRoots :: Map String Node }

data MappingChannel = MappingChannel { mcNtPoints :: Int, mcTPoints :: [Float], mcFaces :: [Int] }

data Mesh = Mesh { mesName :: String, mesNPoints :: Int, mesPoints :: [Float], mesNFaces :: Int, mesFaces :: [Int], mesMapping :: [MappingChannel] }

data NodeTM = NodeTM { ntmName :: String, ntmPos :: Point3D, ntmTm :: [Point3D] }

data Point3D = Point3D Int Int Int

data Material = Material { matName :: String, matDiffuseMap :: String, matAmbientColor :: Point3D, matDiffuseColor :: Point3D, matSpecularColor :: Point3D }

data Node = Node { nodName :: String, nodNodeTM :: NodeTM, nodParent :: Node, nodChildren :: [Node] }

data LightNode = LightNode { lnIntensity :: Float, lnR :: Float, lnG :: Float, lnB :: Float, lnBaseNode :: Node }

data CameraNode = CameraNode { cnTarget :: NodeTM, cnNear :: Float, cnFar :: Float, cnFov :: Float, cnBaseNode :: Node }

data GeomNode = GeomNode { gnMesh :: Mesh, gnMaterialRef :: Int, gnBaseNode :: Node }

main :: IO ()
main = pure ()
