using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EBCDIC.Entities
{
    public class PdorptcyRecord : IDbScriptRecord
    {
        // Always "02" for PDORPTCY (Oil Reporting Cycle)
        public int PdOilRptCycleKey { get; set; }            // PIC 9(04)

        public decimal PdDailyOilProratedAllow { get; set; }    // S9(09) COMP-3
        public decimal PdDailyOilExemptAllow { get; set; }    // S9(09) COMP-3
        public decimal PdDailyCshProratedAllow { get; set; }    // S9(09) COMP-3
        public decimal PdDailyCshExemptAllow { get; set; }    // S9(09) COMP-3
        public decimal PdOilAllowableCycleBbls { get; set; }    // S9(09) COMP-3
        public decimal PdCshLimitCycleMcf { get; set; }    // S9(09) COMP-3

        public short PdOilAllowEffYear { get; set; }           // 9(04)
        public byte PdOilAllowEffMonth { get; set; }           // 9(02)
        public byte PdOilAllowEffDay { get; set; }           // 9(02)

        public short PdOilAllowIssYear { get; set; }           // 9(04)
        public byte PdOilAllowIssMonth { get; set; }           // 9(02)
        public byte PdOilAllowIssDay { get; set; }           // 9(02)

        public decimal PdOilEndingBalance { get; set; }      // S9(09) COMP-3
        public decimal PdPresentOilStatus { get; set; }      // S9(09) COMP-3
        public decimal PdPresentCsghdStatus { get; set; }      // S9(09) COMP-3
        public decimal PdAdjustedOilStatus { get; set; }      // S9(09) COMP-3
        public decimal PdAdjustedCsghdStatus { get; set; }      // S9(09) COMP-3

        /// <summary>
        /// Builds a naive INSERT statement for the PDORPTCY table.
        /// For production code, consider parameterized queries or ORM.
        /// </summary>
        public string ToSqlInsert()
        {
            return $@"
INSERT INTO dbo.PDORPTCY (
    RRC_TAPE_RECORD_ID,
    PD_OIL_RPT_CYCLE_KEY,
    PD_DAILY_OIL_PRORATED_ALLOW,
    PD_DAILY_OIL_EXEMPT_ALLOW,
    PD_DAILY_CSH_PRORATED_ALLOW,
    PD_DAILY_CSH_EXEMPT_ALLOW,
    PD_OIL_ALLOWABLE_CYCLE_BBLS,
    PD_CSH_LIMIT_CYCLE_MCF,
    PD_OIL_ALLOW_EFF_YEAR,
    PD_OIL_ALLOW_EFF_MONTH,
    PD_OIL_ALLOW_EFF_DAY,
    PD_OIL_ALLOW_ISS_YEAR,
    PD_OIL_ALLOW_ISS_MONTH,
    PD_OIL_ALLOW_ISS_DAY,
    PD_OIL_ENDING_BALANCE,
    PD_PRESENT_OIL_STATUS,
    PD_PRESENT_CSGHD_STATUS,
    PD_ADJUSTED_OIL_STATUS,
    PD_ADJUSTED_CSGHD_STATUS
)
VALUES (
    '02',
    {PdOilRptCycleKey},
    {PdDailyOilProratedAllow},
    {PdDailyOilExemptAllow},
    {PdDailyCshProratedAllow},
    {PdDailyCshExemptAllow},
    {PdOilAllowableCycleBbls},
    {PdCshLimitCycleMcf},
    {PdOilAllowEffYear},
    {PdOilAllowEffMonth},
    {PdOilAllowEffDay},
    {PdOilAllowIssYear},
    {PdOilAllowIssMonth},
    {PdOilAllowIssDay},
    {PdOilEndingBalance},
    {PdPresentOilStatus},
    {PdPresentCsghdStatus},
    {PdAdjustedOilStatus},
    {PdAdjustedCsghdStatus}
);";
        }
    }

}
