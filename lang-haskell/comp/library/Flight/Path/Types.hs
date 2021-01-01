module Flight.Path.Types
    ( IxTask(..)
    , DotFolder(..)
    , AltDot(..)
    , FileType(..)
    , FileShape(..)

    , CompDir(..)
    , TaskDir(..)
    , UnpackTrackDir(..)
    , AlignTimeDir(..)
    , DiscardFurtherDir(..)
    , AreaStepDir(..)
    , PegThenDiscardDir(..)

    , CompInputFile(..)
    , TaskInputFile(..)
    , ScoringInputFiles

    , IgcFile(..)
    , KmlFile(..)
    , FsdbFile(..)
    , CleanFsdbFile(..)
    , TrimFsdbFile(..)
    , FsdbXml(..)
    , AltArrivalFile(..)
    , AltLandoutFile(..)
    , AltRouteFile(..)
    , AltScoreFile(..)
    , TaskLengthFile(..)
    , FlyTimeFile(..)
    , CrossZoneFile(..)
    , TagZoneFile(..)
    , PegFrameFile(..)
    , UnpackTrackFile(..)
    , AlignTimeFile(..)
    , DiscardFurtherFile(..)
    , AreaStepFile(..)
    , PegThenDiscardFile(..)
    , LeadAreaFile(..)
    , MaskArrivalFile(..)
    , MaskEffortFile(..)
    , MaskLeadFile(..)
    , MaskReachFile(..)
    , MaskSpeedFile(..)
    , BonusReachFile(..)
    , LandOutFile(..)
    , FarOutFile(..)
    , GapPointFile(..)
    ) where

-- | 1-based indices of a task in a competition.
newtype IxTask = IxTask Int
    deriving (Eq, Show)
    deriving newtype Num

data DotFolder
    = DotRoot -- ^ Root is the same folder as the *.fsdb.
    | DotFt -- ^ The dot folder for flare-timing, .flare-timing.
    | DotFs -- ^ The dot folder for FS, .flight-system.
    | DotAs -- ^ The dot folder for airScore, .air-score.
    deriving Show

-- | Alternative dot folders than .flare-timing and root.
data AltDot = AltFs | AltAs

data FileType
    = Fsdb -- ^ A *.fsdb file, usually obtained from FS.
    | CleanFsdb -- ^ An XML comp file, cleaned of personal details.
    | TrimFsdb
    -- ^ An XML comp file trimmed down to just the elements and attributes that
    -- flare-timing needs for scoring.

    | Kml
    | Igc

    | CompInput
    | TaskInput
    | TaskLength
    | FlyTime
    | CrossZone
    | TagZone
    | PegFrame
    | MaskArrival
    | MaskEffort
    | MaskLead
    | MaskReach
    | MaskSpeed
    | BonusReach
    | LeadArea
    | LandOut
    | FarOut
    | GapPoint

    | UnpackTrack
    | AlignTime
    | DiscardFurther
    | PegThenDiscard
    | AreaStep

    | AltArrival AltDot
    | AltLandout AltDot
    | AltRoute AltDot
    | AltScore AltDot

type ScoringInputFiles = (CompInputFile, [TaskInputFile])

data FileShape
    = Ext FilePath
    | DotDirName FilePath DotFolder

-- | The path to a competition arrival file, scored elsewhere.
newtype AltArrivalFile = AltArrivalFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a competition effort file, scored elsewhere.
newtype AltLandoutFile = AltLandoutFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a competition optimal route file, scored elsewhere.
newtype AltRouteFile = AltRouteFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a competition score file, scored elsewhere.
newtype AltScoreFile = AltScoreFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a *.igc file.
newtype IgcFile = IgcFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a *.kml file.
newtype KmlFile = KmlFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a competition *.fsdb file.
newtype FsdbFile = FsdbFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a competition *.clean-fsdb.xml file, an *.fsdb clean of
-- potentially sensitive private information.
newtype CleanFsdbFile = CleanFsdbFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a competition *.trim-fsdb.xml file, an *.fsdb filtered for use
-- by flare-timing..
newtype TrimFsdbFile = TrimFsdbFile FilePath
    deriving Eq
    deriving newtype Show

-- | The XML string contents of a *.fsdb file.
newtype FsdbXml = FsdbXml String
    deriving Eq
    deriving newtype Show

-- | The directory path of a competition file.
newtype CompDir = CompDir FilePath
    deriving Eq
    deriving newtype Show

-- | The directory path of a task.
newtype TaskDir = TaskDir FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a competition file.
newtype CompInputFile = CompInputFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a task file.
newtype TaskInputFile = TaskInputFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a task length file.
newtype TaskLengthFile = TaskLengthFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a fly time file.
newtype FlyTimeFile = FlyTimeFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a cross zone file.
newtype CrossZoneFile = CrossZoneFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a tag zone file.
newtype TagZoneFile = TagZoneFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a stop task file.
newtype PegFrameFile = PegFrameFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to an unpack track directory for a single task.
newtype UnpackTrackDir = UnpackTrackDir FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a discard further directory for a single task.
newtype DiscardFurtherDir = DiscardFurtherDir FilePath
    deriving Eq
    deriving newtype Show

-- | The path to an align time directory for a single task.
newtype AlignTimeDir = AlignTimeDir FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a area step directory for a single task.
newtype AreaStepDir = AreaStepDir FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a peg then discard directory for a single task.
newtype PegThenDiscardDir = PegThenDiscardDir FilePath
    deriving Eq
    deriving newtype Show

-- | The path to as unpack track file.
newtype UnpackTrackFile = UnpackTrackFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to an align time file.
newtype AlignTimeFile = AlignTimeFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a discard further file.
newtype DiscardFurtherFile = DiscardFurtherFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a area step file.
newtype AreaStepFile = AreaStepFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a peg then discard file.
newtype PegThenDiscardFile = PegThenDiscardFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a leading area file.
newtype LeadAreaFile = LeadAreaFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a mask arrival file.
newtype MaskArrivalFile = MaskArrivalFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a mask effort file.
newtype MaskEffortFile = MaskEffortFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a mask lead file.
newtype MaskLeadFile = MaskLeadFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a mask reach file.
newtype MaskReachFile = MaskReachFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a mask speed file.
newtype MaskSpeedFile = MaskSpeedFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a mask reach with altitude bonus distance file.
newtype BonusReachFile = BonusReachFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a land out file.
newtype LandOutFile = LandOutFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a far out file.
newtype FarOutFile = FarOutFile FilePath
    deriving Eq
    deriving newtype Show

-- | The path to a gap point file.
newtype GapPointFile = GapPointFile FilePath
    deriving Eq
    deriving newtype Show
