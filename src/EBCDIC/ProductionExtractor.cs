using EBCDIC.Entities;
using Microsoft.Data.SqlClient;
using Microsoft.Extensions.Logging;
using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EBCDIC
{
    public class ProductionExtractor
    {
        private readonly ILogger _logger;

        public ProductionExtractor(ILogger logger)
        {
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }

        public void ProcessRecord(string recordId, Encoding encoding, byte[] recordBytes)
        {
            switch (recordId)
            {
                case "01":
                    ExtractPdrootOil01(recordId, encoding, recordBytes);
                    break;
                case "02":
                    ExtractPdorptcy02(recordId, encoding, recordBytes);
                    break;
                case "03":
                    ExtractPdoprod03(recordId, encoding, recordBytes);
                    break;
                case "04":
                    ExtractPdormvds04(recordId, encoding, recordBytes);
                    break;
                case "05":
                    ExtractPdodsp05(recordId, encoding, recordBytes);
                    break;
                case "11":
                    ExtractPdgrptcy14(recordId, encoding, recordBytes);
                    break;
                case "13":
                    ExtractPdoprval13(recordId, encoding, recordBytes);
                    break;
                case "15":
                    ExtractPdgprod15(recordId, encoding, recordBytes);
                    break;
                // Add more cases for other record types
                default:
                    // Log unknown record ID
                    _logger.LogWarning($"Unknown record ID: {recordId}");
                    break;
            }
        }

        //05
        public static PdodspRecord ExtractPdodsp05(
        string recordId,
        Encoding encoding,
        byte[] recordBytes)
        {
            // 1) Validate
            if (recordId != "05")
                throw new InvalidOperationException(
                    $"Not a PDODSP record. Expected '05', got '{recordId}'.");

            // 2) Create entity
            var record = new PdodspRecord();

            // 3) Parse fields
            // Per the PDF snippet for PDODSP:
            //  [0..1]  => "05"  (RRC_TAPE_RECORD_ID)
            //  [2..3]  => PD-OIL-DISPOSITION-CODE (PIC 9(2))
            //  [4..8]  => PD-OIL-DISPOSITION-AMOUNT (5 bytes COMP-3)
            //  [9..end] => filler
            string codeStr = encoding.GetString(recordBytes, 2, 2);
            if (short.TryParse(codeStr, out short codeVal))
                record.PdOilDispositionCode = codeVal;

            // Decode the COMP-3 amount (5 bytes => offset 4..8)
            record.PdOilDispositionAmount = DecodeComp3(recordBytes, 4, 5);

            return record;
        }

        //04
        public static PdormvdsRecord ExtractPdormvds04(
        string recordId,
        Encoding encoding,
        byte[] recordBytes)
        {
            // Validate
            if (recordId != "04")
            {
                throw new InvalidOperationException(
                    $"Not a PDORMVDS record. Expected '04', got '{recordId}'.");
            }

            // Create entity
            var record = new PdormvdsRecord();

            // According to the PDF snippet:
            //   [0..1]  => "04"
            //   [2]     => PD-O-REMOVED-CODE-6-DISCREP    X(1)
            //   [3]     => PD-O-REMOVED-R-3-DISCREP       X(1)
            //   [4]     => PD-O-REMOVED-NET-OIL-DISCREP   X(1)
            //   [5]     => PD-O-REMOVED-LSE-SEV-DISCREP   X(1)
            //   [6]     => PD-O-REMOVED-OTHER-DISCREP     X(1)
            //   [7]     => PD-O-REMOVED-LOST-OIL-DISCREP  X(1)
            //   [8]     => PD-O-REMOVED-CIRC-OIL-DISCREP  X(1)
            //   [9]     => PD-O-REMOVED-PRD-INACT-DISCREP X(1)
            //   [10]    => PD-O-REMOVED-FILE-COMM-DISCREP X(1)
            //   [11..end] => filler

            record.PdORemovedCode6Discrep = encoding.GetString(recordBytes, 2, 1);
            record.PdORemovedR3Discrep = encoding.GetString(recordBytes, 3, 1);
            record.PdORemovedNetOilDiscrep = encoding.GetString(recordBytes, 4, 1);
            record.PdORemovedLseSevDiscrep = encoding.GetString(recordBytes, 5, 1);
            record.PdORemovedOtherDiscrep = encoding.GetString(recordBytes, 6, 1);
            record.PdORemovedLostOilDiscrep = encoding.GetString(recordBytes, 7, 1);
            record.PdORemovedCircOilDiscrep = encoding.GetString(recordBytes, 8, 1);
            record.PdORemovedPrdInactDiscrep = encoding.GetString(recordBytes, 9, 1);
            record.PdORemovedFileCommDiscrep = encoding.GetString(recordBytes, 10, 1);

            return record;
        }

        //02
        public static PdorptcyRecord ExtractPdorptcy02(
        string recordId,
        Encoding encoding,
        byte[] recordBytes)
        {
            // 1) Validate recordId
            if (recordId != "02")
            {
                throw new InvalidOperationException(
                    $"Not a PDORPTCY record. Expected '02', got '{recordId}'.");
            }

            // 2) Create the entity
            var record = new PdorptcyRecord();

            // 3) Parse numeric fields
            // PD_OIL_RPT_CYCLE_KEY (offset 3..6, 4 digits)
            string cycleKeyStr = encoding.GetString(recordBytes, 3, 4);
            if (int.TryParse(cycleKeyStr, out int parsedCycleKey))
                record.PdOilRptCycleKey = parsedCycleKey;

            // Next are COMP-3 fields (5 bytes each)
            record.PdDailyOilProratedAllow = DecodeComp3(recordBytes, 7, 5);
            record.PdDailyOilExemptAllow = DecodeComp3(recordBytes, 12, 5);
            record.PdDailyCshProratedAllow = DecodeComp3(recordBytes, 17, 5);
            record.PdDailyCshExemptAllow = DecodeComp3(recordBytes, 22, 5);
            record.PdOilAllowableCycleBbls = DecodeComp3(recordBytes, 27, 5);
            record.PdCshLimitCycleMcf = DecodeComp3(recordBytes, 32, 5);

            // Effective Date: year(4), month(2), day(2)
            string effYearStr = encoding.GetString(recordBytes, 37, 4);
            string effMonthStr = encoding.GetString(recordBytes, 41, 2);
            string effDayStr = encoding.GetString(recordBytes, 43, 2);

            if (short.TryParse(effYearStr, out short effYr)) record.PdOilAllowEffYear = effYr;
            if (byte.TryParse(effMonthStr, out byte effMo)) record.PdOilAllowEffMonth = effMo;
            if (byte.TryParse(effDayStr, out byte effDy)) record.PdOilAllowEffDay = effDy;

            // Issue Date: year(4), month(2), day(2)
            string issYearStr = encoding.GetString(recordBytes, 45, 4);
            string issMonthStr = encoding.GetString(recordBytes, 49, 2);
            string issDayStr = encoding.GetString(recordBytes, 51, 2);

            if (short.TryParse(issYearStr, out short issYr)) record.PdOilAllowIssYear = issYr;
            if (byte.TryParse(issMonthStr, out byte issMo)) record.PdOilAllowIssMonth = issMo;
            if (byte.TryParse(issDayStr, out byte issDy)) record.PdOilAllowIssDay = issDy;

            // Additional COMP-3 fields
            record.PdOilEndingBalance = DecodeComp3(recordBytes, 53, 5);
            record.PdPresentOilStatus = DecodeComp3(recordBytes, 58, 5);
            record.PdPresentCsghdStatus = DecodeComp3(recordBytes, 63, 5);
            record.PdAdjustedOilStatus = DecodeComp3(recordBytes, 68, 5);
            record.PdAdjustedCsghdStatus = DecodeComp3(recordBytes, 73, 5);

            return record;
        }

        //03
        public PdoprodRecord ExtractPdoprod03(string recordId, Encoding encoding, byte[] recordBytes)
        {
            // Typically, recordId should be "03" if we’re parsing PDOPROD
            // But you could check:
            // if (recordId != "03") throw new InvalidOperationException("Not a PDOPROD record!");

            var result = new PdoprodRecord();

            // 1) Corrected report flag (offset 2, length 1)
            result.CorrectedReportFlag = encoding.GetString(recordBytes, 2, 1);

            // 2) Production Amount (offset 7, length 5 for COMP-3)
            result.ProductionAmount = DecodeComp3(recordBytes, 7, 5);

            // 3) Casinghead Gas Amount (offset 12, length 5 for COMP-3)
            result.CasingheadGasAmount = DecodeComp3(recordBytes, 12, 5);

            // 4) Casinghead Gas Lift (offset 17, length 5 for COMP-3)
            result.CasingheadGasLift = DecodeComp3(recordBytes, 17, 5);

            // 5) Batch Number (offset 22, length 3, PIC X(3))
            result.BatchNumber = encoding.GetString(recordBytes, 22, 3);

            // 6) Item Number (offset 25, length 4, PIC 9(4))
            string itemNumberStr = encoding.GetString(recordBytes, 25, 4);
            if (int.TryParse(itemNumberStr, out int parsedItemNumber))
                result.ItemNumber = parsedItemNumber;

            // 7) Posting Year (offset 29, length 4)
            string postingYearStr = encoding.GetString(recordBytes, 29, 4);
            if (short.TryParse(postingYearStr, out short parsedYear))
                result.PostingYear = parsedYear;

            // 8) Posting Month (offset 33, length 2)
            string postingMonthStr = encoding.GetString(recordBytes, 33, 2);
            if (byte.TryParse(postingMonthStr, out byte parsedMonth))
                result.PostingMonth = parsedMonth;

            // 9) Posting Day (offset 35, length 2)
            string postingDayStr = encoding.GetString(recordBytes, 35, 2);
            if (byte.TryParse(postingDayStr, out byte parsedDay))
                result.PostingDay = parsedDay;

            // 10) Filed by EDI (offset 37, length 1)
            result.FiledByEdiFlag = encoding.GetString(recordBytes, 37, 1);

            // Done! Return the parsed object
            return result;
        }

        //01
        public static PdrootOilRecord ExtractPdrootOil01(string recordId, Encoding encoding, byte[] recordBytes)
        {
            // Ensure we're dealing with Record ID = "01"
            if (recordId != "01")
                throw new InvalidOperationException($"Not a PDROOT_OIL record. Expected '01', got '{recordId}'.");

            // Create an instance to fill
            var record = new PdrootOilRecord();

            // Offsets (0-based) are based on the PDF definitions:
            // [0..1]   = RRC-TAPE-RECORD-ID    (already read as recordId)
            // [2]      = PD-OIL-CODE (X(1))
            // [3..4]   = PD-OIL-DISTRICT (9(2))
            // [5..10]  = PD-OIL-LEASE-NBR (9(6))
            // [11..15] = PD-MOVABLE-BALANCE (S9(09) COMP-3) => 5 bytes
            // [16..20] = PD-BEGINNING-OIL-STATUS (S9(09) COMP-3) => 5 bytes
            // [21..25] = PD-BEGINNING-CSGHD-STATUS (S9(09) COMP-3) => 5 bytes
            // [26..30] = PD-OIL-OLDEST-EOM-BALANCE (S9(09) COMP-3) => 5 bytes
            // The rest is filler (X(21) + X(50)).

            // 1) PD_OIL_CODE (offset 2, length 1)
            record.PdOilCode = encoding.GetString(recordBytes, 2, 1);

            // 2) PD_OIL_DISTRICT (offset 3, length 2) => parse numeric
            string districtStr = encoding.GetString(recordBytes, 3, 2);
            if (short.TryParse(districtStr, out short district))
                record.PdOilDistrict = district;

            // 3) PD_OIL_LEASE_NBR (offset 5, length 6) => parse numeric
            string leaseNbrStr = encoding.GetString(recordBytes, 5, 6);
            if (int.TryParse(leaseNbrStr, out int leaseNbr))
                record.PdOilLeaseNbr = leaseNbr;

            // 4) PD_MOVABLE_BALANCE (offset 11, length 5) => COMP-3
            record.PdMovableBalance = DecodeComp3(recordBytes, 11, 5);

            // 5) PD_BEGINNING_OIL_STATUS (offset 16, length 5) => COMP-3
            record.PdBeginningOilStatus = DecodeComp3(recordBytes, 16, 5);

            // 6) PD_BEGINNING_CSGHD_STATUS (offset 21, length 5) => COMP-3
            record.PdBeginningCsghdStatus = DecodeComp3(recordBytes, 21, 5);

            // 7) PD_OIL_OLDEST_EOM_BALANCE (offset 26, length 5) => COMP-3
            record.PdOilOldestEomBalance = DecodeComp3(recordBytes, 26, 5);

            return record;
        }

        //15
        public static PdgprodRecord ExtractPdgprod15(
        string recordId,
        Encoding encoding,
        byte[] recordBytes)
        {
            // 1) Ensure correct record type
            if (recordId != "15")
                throw new InvalidOperationException(
                    $"Not a PDGPROD record. Expected '15', got '{recordId}'.");

            // 2) Create the entity
            var record = new PdgprodRecord();

            // Offsets (0-based) are based on the PDF definitions for PDGPROD:
            //  [0..1]   -> RRC-TAPE-RECORD-ID ("15")
            //  [3..7]   -> PD_GAS_PROD, 5 bytes COMP-3
            //  [8..12]  -> PD_GAS_LIFT_GAS_INJECTED, 5 bytes COMP-3
            //  [13..17] -> PD_COND_PROD, 5 bytes COMP-3
            //  [18..20] -> PD_GAS_BATCH_NUMBER (3 chars)
            //  [21..24] -> PD_GAS_ITEM_NUMBER (4 digits)
            //  [25..28] -> PD_GAS_POSTING_YEAR (4 digits)
            //  [29..30] -> PD_GAS_POSTING_MONTH (2 digits)
            //  [31..32] -> PD_GAS_POSTING_DAY (2 digits)
            // Single-byte flags from offset 33 onward:
            //  33: PD_G-REMOVED-FLARING-DISCREP
            //  34: PD_G-REMOVED-SEALED-DISCREP
            //  35: PD_G-REMOVED-H-8-DISCREP
            //  36: PD_G-REMOVED-CIRC-OIL-DISCREP
            //  37: PD_G-REMOVED-OTHER-DISCREP
            //  38: PD_G-REMOVED-CODE4-DISCREP
            //  39: PD_GAS-CORRECTED-REPORT-FLAG
            //  40: PD_G-REMOVED-CODE-6-DISCREP
            //  41: PD_GAS-FILED-BY-EDI-FLAG
            //  42: PD_G-REMOVED-WELL-PROD-DISCREP
            //  43: PD_G-REMOVED-CON-EXT-L-DISCREP
            //  44: PD_G-REMOVED-CON-INSUF-DISCREP
            //  45: PD_G-REMOVED-CODE-3-DISCREP
            //  [46..end] -> filler

            // 3) Decode COMP-3 fields
            record.GasProd = DecodeComp3(recordBytes, 3, 5);
            record.GasLiftGasInjected = DecodeComp3(recordBytes, 8, 5);
            record.CondProd = DecodeComp3(recordBytes, 13, 5);

            // 4) Extract text/numeric fields
            record.GasBatchNumber = encoding.GetString(recordBytes, 18, 3);

            // PD_GAS_ITEM_NUMBER (21..24)
            string itemNumberStr = encoding.GetString(recordBytes, 21, 4);
            if (int.TryParse(itemNumberStr, out int itemNumber))
                record.GasItemNumber = itemNumber;

            // PD_GAS_POSTING_YEAR (25..28)
            string yearStr = encoding.GetString(recordBytes, 25, 4);
            if (short.TryParse(yearStr, out short year))
                record.GasPostingYear = year;

            // PD_GAS_POSTING_MONTH (29..30)
            string monthStr = encoding.GetString(recordBytes, 29, 2);
            if (byte.TryParse(monthStr, out byte month))
                record.GasPostingMonth = month;

            // PD_GAS_POSTING_DAY (31..32)
            string dayStr = encoding.GetString(recordBytes, 31, 2);
            if (byte.TryParse(dayStr, out byte day))
                record.GasPostingDay = day;

            // 5) Single-char flags from offset 33 onward
            record.RemovedFlaringDiscrep = encoding.GetString(recordBytes, 33, 1);
            record.RemovedSealedDiscrep = encoding.GetString(recordBytes, 34, 1);
            record.RemovedH8Discrep = encoding.GetString(recordBytes, 35, 1);
            record.RemovedCircOilDiscrep = encoding.GetString(recordBytes, 36, 1);
            record.RemovedOtherDiscrep = encoding.GetString(recordBytes, 37, 1);
            record.RemovedCode4Discrep = encoding.GetString(recordBytes, 38, 1);
            record.GasCorrectedReportFlag = encoding.GetString(recordBytes, 39, 1);
            record.RemovedCode6Discrep = encoding.GetString(recordBytes, 40, 1);
            record.GasFiledByEdiFlag = encoding.GetString(recordBytes, 41, 1);
            record.RemovedWellProdDiscrep = encoding.GetString(recordBytes, 42, 1);
            record.RemovedConExtLDiscrep = encoding.GetString(recordBytes, 43, 1);
            record.RemovedConInsufDiscrep = encoding.GetString(recordBytes, 44, 1);
            record.RemovedCode3Discrep = encoding.GetString(recordBytes, 45, 1);

            return record;
        }


        //14
        public static PdgrptcyRecord ExtractPdgrptcy14(
        string recordId,
        Encoding encoding,
        byte[] recordBytes)
        {
            // 1) Validate
            if (recordId != "14")
            {
                throw new InvalidOperationException(
                    $"Not a PDGRPTCY record. Expected '14', got '{recordId}'.");
            }

            // 2) Create the entity
            var record = new PdgrptcyRecord();

            // Offsets (0-based), from the PDF snippet:
            //   [0..1]   => RRC-TAPE-RECORD-ID   ("14")
            //   [3..6]   => PD-GAS-REPORT-CYCLE-KEY (4 digits)
            //   [7..11]  => PD-GAS-BALANCING-ALLOW-AMT (5 bytes COMP-3)
            //   [12..13] => PD-GAS-BALANCING-ALLOW-CODE (2 chars, X(2))
            //   [14]     => PD-GAS-EXCEPTION-TWICE-ALLOW (1 char)
            //   [15]     => PD-GAS-HIGHEST-DAILY-CYCLES (1 digit)
            //   [16..20] => PD-GAS-EXCEPT-HIGH-DAY-AMOUNT (5 bytes COMP-3)
            //   [21..23] => PD-GAS-REDUCED-RATE (3 chars, PIC 9(03))
            //   [24]     => PD-GAS-REDUCED-RATE-CODE (1 char)
            //   [25..29] => PD-COND-ENDING-BALANCE (5 bytes COMP-3)
            //   [30..34] => PD-G9-INJECTION-AMOUNT (5 bytes COMP-3)
            //   [35..39] => PD-GAS-CUMU-CYCLE-STATUS (5 bytes COMP-3)
            //   [40..44] => PD-GAS-CUMULATIVE-OVERAGE (5 bytes COMP-3)
            //   [45..49] => PD-GAS-OVERAGE-TRANSFER (5 bytes COMP-3)
            //   [50]     => PD-GAS-NO-PAST-PRODUCTION-FLAG (1 char)
            //   [51]     => PD-GAS-NO-HIGHEST-DAILY-FLAG (1 char)
            //   [52]     => PD-GAS-CANCEL-UNDERAGE-FLAG (1 char)
            //   [53]     => PD-GAS-PAST-PRODUCTION-CYCLES (1 digit)
            //   [54..58] => PD-GAS-EXCEPT-PAST-PROD-AMOUNT (5 bytes COMP-3)
            //   [59..63] => PD-LIQUID-CUMU-CYCLE-STATUS (5 bytes COMP-3)
            //   [64..68] => PD-G9-PLANT-LIQUID (5 bytes COMP-3)
            //   [69]     => PD-G9-INJECTION-CREDIT-CODE (1 char)
            //   [70]     => PD-GAS-ON-SHUT-IN-LIST-FLAG (1 char)
            //   [71..75] => PD-WELL-CAPABILITY (5 bytes COMP-3)
            //   [76..80] => PD-WELL-MONTHLY-CAPABILITY (5 bytes COMP-3)
            //   [81..end] => filler

            // 3) Parse fields
            // PD_GAS_REPORT_CYCLE_KEY (offset 3..6)
            string cycleKeyStr = encoding.GetString(recordBytes, 3, 4);
            if (int.TryParse(cycleKeyStr, out int cycleKey))
                record.GasReportCycleKey = cycleKey;

            record.GasBalancingAllowAmt = DecodeComp3(recordBytes, 7, 5);

            record.GasBalancingAllowCode = encoding.GetString(recordBytes, 12, 2);

            record.GasExceptionTwiceAllow = encoding.GetString(recordBytes, 14, 1);

            // Highest Daily Cycles (offset 15, 1 digit)
            string hdcStr = encoding.GetString(recordBytes, 15, 1);
            if (byte.TryParse(hdcStr, out byte hdcVal))
                record.GasHighestDailyCycles = hdcVal;

            record.GasExceptHighDayAmount = DecodeComp3(recordBytes, 16, 5);

            // GasReducedRate (offset 21..23, 3 digits)
            string reducedRateStr = encoding.GetString(recordBytes, 21, 3);
            if (short.TryParse(reducedRateStr, out short rrVal))
                record.GasReducedRate = rrVal;

            record.GasReducedRateCode = encoding.GetString(recordBytes, 24, 1);

            record.CondEndingBalance = DecodeComp3(recordBytes, 25, 5);
            record.G9InjectionAmount = DecodeComp3(recordBytes, 30, 5);
            record.GasCumuCycleStatus = DecodeComp3(recordBytes, 35, 5);
            record.GasCumulativeOverage = DecodeComp3(recordBytes, 40, 5);
            record.GasOverageTransfer = DecodeComp3(recordBytes, 45, 5);

            record.GasNoPastProductionFlag = encoding.GetString(recordBytes, 50, 1);
            record.GasNoHighestDailyFlag = encoding.GetString(recordBytes, 51, 1);
            record.GasCancelUnderageFlag = encoding.GetString(recordBytes, 52, 1);

            // PD-GAS-PAST-PRODUCTION-CYCLES (offset 53, 1 digit)
            string ppcStr = encoding.GetString(recordBytes, 53, 1);
            if (byte.TryParse(ppcStr, out byte ppcVal))
                record.GasPastProductionCycles = ppcVal;

            record.GasExceptPastProdAmount = DecodeComp3(recordBytes, 54, 5);
            record.LiquidCumuCycleStatus = DecodeComp3(recordBytes, 59, 5);
            record.G9PlantLiquid = DecodeComp3(recordBytes, 64, 5);

            record.G9InjectionCreditCode = encoding.GetString(recordBytes, 69, 1);
            record.GasOnShutInListFlag = encoding.GetString(recordBytes, 70, 1);

            record.WellCapability = DecodeComp3(recordBytes, 71, 5);
            record.WellMonthlyCapability = DecodeComp3(recordBytes, 76, 5);

            return record;
        }

        //13
        public static PdoprvalRecord ExtractPdoprval13(
        string recordId,
        Encoding encoding,
        byte[] recordBytes)
        {
            // 1) Validate
            if (recordId != "13")
            {
                throw new InvalidOperationException(
                    $"Not a PDOPRVAL record. Expected '13', got '{recordId}'.");
            }

            var record = new PdoprvalRecord();

            // Offsets from the PDF (example for a 102-byte record):
            //  [0..1]   => RRC-TAPE-RECORD-ID ("13")
            //  [2..6]   => PD-OIL-ALLOW-DAILY-BBLS-HIST (5 bytes, S9(09) COMP-3)
            //  [7..11]  => PD-GAS-LIMIT-DAILY-MCF-HIST  (5 bytes, S9(09) COMP-3)
            //  [12..15] => PD-OIL-ALLOW-EFF-YEAR-HIST   (4 digits)
            //  [16..17] => PD-OIL-ALLOW-EFF-MONTH-HIST  (2 digits)
            //  [18..19] => PD-OIL-ALLOW-EFF-DAY-HIST    (2 digits)
            //  [20..23] => PD-OIL-ALLOW-ISS-YEAR-HIST   (4 digits)
            //  [24..25] => PD-OIL-ALLOW-ISS-MONTH-HIST  (2 digits)
            //  [26..27] => PD-OIL-ALLOW-ISS-DAY-HIST    (2 digits)
            //  [28..32] => PD-OIL-ALLOW-CYCLE-BBLS-HIST (5 bytes, S9(09) COMP-3)
            //  [33..37] => PD-GAS-LIMIT-CYCLE-MCF-HIST  (5 bytes, S9(09) COMP-3)
            //  [38..51] => filler (14 bytes)
            //  [52..101]=> RRC-TAPE-FILLER (50 bytes)

            // 2) Parse COMP-3 fields
            record.PdOilAllowDailyBblsHist = DecodeComp3(recordBytes, 2, 5);
            record.PdGasLimitDailyMcfHist = DecodeComp3(recordBytes, 7, 5);

            // 3) Dates for EFF date
            string effYearStr = encoding.GetString(recordBytes, 12, 4);
            string effMonthStr = encoding.GetString(recordBytes, 16, 2);
            string effDayStr = encoding.GetString(recordBytes, 18, 2);

            // Convert them
            if (short.TryParse(effYearStr, out short effYr)) record.PdOilAllowEffYearHist = effYr;
            if (byte.TryParse(effMonthStr, out byte effMo)) record.PdOilAllowEffMonthHist = effMo;
            if (byte.TryParse(effDayStr, out byte effDy)) record.PdOilAllowEffDayHist = effDy;

            // 4) Dates for ISSUE date
            string issYearStr = encoding.GetString(recordBytes, 20, 4);
            string issMonthStr = encoding.GetString(recordBytes, 24, 2);
            string issDayStr = encoding.GetString(recordBytes, 26, 2);

            if (short.TryParse(issYearStr, out short issYr)) record.PdOilAllowIssYearHist = issYr;
            if (byte.TryParse(issMonthStr, out byte issMo)) record.PdOilAllowIssMonthHist = issMo;
            if (byte.TryParse(issDayStr, out byte issDy)) record.PdOilAllowIssDayHist = issDy;

            // 5) Additional COMP-3 fields
            record.PdOilAllowCycleBblsHist = DecodeComp3(recordBytes, 28, 5);
            record.PdGasLimitCycleMcfHist = DecodeComp3(recordBytes, 33, 5);

            // The rest is filler
            return record;
        }


        /// <summary>
        /// Example method to decode a PIC S9(n) COMP-3 (packed decimal) field
        /// from a byte array.
        /// lengthInBytes is how many bytes the packed field occupies.
        /// </summary>
        static decimal DecodeComp3(byte[] buffer, int offset, int lengthInBytes)
        {
            // Each byte has two nibbles => 1 nibble holds 1 digit, 
            // except last nibble holds sign. 
            // E.g. PIC S9(9) COMP-3 typically uses 5 bytes -> 9 digits + sign nibble
            //
            // High-level approach:
            // 1) Extract nibbles into a string of digits
            // 2) Check last nibble for sign (0xC or 0xF => +, 0xD => -)
            // 3) Convert to decimal

            // Example Implementation:
            StringBuilder digits = new StringBuilder(lengthInBytes * 2 - 1);
            bool isNegative = false;

            for (int i = 0; i < lengthInBytes; i++)
            {
                // current byte
                byte b = buffer[offset + i];

                // high nibble
                int high = (b & 0xF0) >> 4;
                // low nibble
                int low = (b & 0x0F);

                if (i < lengthInBytes - 1)
                {
                    // For all but the last byte, both nibbles are digits
                    digits.Append(high.ToString());
                    digits.Append(low.ToString());
                }
                else
                {
                    // In the last byte, high nibble is a digit, low nibble is the sign
                    digits.Append(high.ToString());

                    // Sign nibble:
                    switch (low)
                    {
                        case 0xD: // negative
                            isNegative = true;
                            break;
                            // 0xC or 0xF => positive
                            // others => sometimes used for unsigned
                    }
                }
            }

            if (digits.Length == 0)
                return 0m;

            decimal value = decimal.Parse(digits.ToString());
            if (isNegative) value = -value;

            return value;
        }

        private static string GetConnectionString()
        {
            return "Server=.\\SQLEXPRESS;Database=EBCDIC;Integrated Security=True;TrustServerCertificate=True";
        }
        private static IDbConnection GetConnection()
        {
            return new SqlConnection(GetConnectionString());
        }
        private static T RunSync<T>(Func<Task<T>> func)
        {
            return Task.Run(func).GetAwaiter().GetResult();
        }
    }
}
