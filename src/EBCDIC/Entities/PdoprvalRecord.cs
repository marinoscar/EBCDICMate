using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EBCDIC.Entities
{
    public class PdoprvalRecord
    {
        // Always "13" for PDOPRVAL
        public decimal PdOilAllowDailyBblsHist { get; set; } // S9(09) COMP-3
        public decimal PdGasLimitDailyMcfHist { get; set; } // S9(09) COMP-3

        public short PdOilAllowEffYearHist { get; set; } // 9(04)
        public byte PdOilAllowEffMonthHist { get; set; } // 9(02)
        public byte PdOilAllowEffDayHist { get; set; } // 9(02)

        public short PdOilAllowIssYearHist { get; set; } // 9(04)
        public byte PdOilAllowIssMonthHist { get; set; } // 9(02)
        public byte PdOilAllowIssDayHist { get; set; } // 9(02)

        public decimal PdOilAllowCycleBblsHist { get; set; } // S9(09) COMP-3
        public decimal PdGasLimitCycleMcfHist { get; set; } // S9(09) COMP-3

        /// <summary>
        /// Naive SQL INSERT for PDOPRVAL. In real apps, prefer parameterized queries / ORM.
        /// </summary>
        public string ToSqlInsert()
        {
            return $@"
INSERT INTO dbo.PDOPRVAL (
    RRC_TAPE_RECORD_ID,
    PD_OIL_ALLOW_DAILY_BBLS_HIST,
    PD_GAS_LIMIT_DAILY_MCF_HIST,
    PD_OIL_ALLOW_EFF_YEAR_HIST,
    PD_OIL_ALLOW_EFF_MONTH_HIST,
    PD_OIL_ALLOW_EFF_DAY_HIST,
    PD_OIL_ALLOW_ISS_YEAR_HIST,
    PD_OIL_ALLOW_ISS_MONTH_HIST,
    PD_OIL_ALLOW_ISS_DAY_HIST,
    PD_OIL_ALLOW_CYCLE_BBLS_HIST,
    PD_GAS_LIMIT_CYCLE_MCF_HIST
)
VALUES (
    '13',
    {PdOilAllowDailyBblsHist},
    {PdGasLimitDailyMcfHist},
    {PdOilAllowEffYearHist},
    {PdOilAllowEffMonthHist},
    {PdOilAllowEffDayHist},
    {PdOilAllowIssYearHist},
    {PdOilAllowIssMonthHist},
    {PdOilAllowIssDayHist},
    {PdOilAllowCycleBblsHist},
    {PdGasLimitCycleMcfHist}
);";
        }
    }

}
