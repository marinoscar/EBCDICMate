using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EBCDIC.Entities
{
    public class PdgrptcyRecord : IDbScriptRecord
    {
        // Record ID always "14" for PDGRPTCY
        public int GasReportCycleKey { get; set; }  // PIC 9(4)
        public decimal GasBalancingAllowAmt { get; set; }  // S9(09) COMP-3
        public string GasBalancingAllowCode { get; set; }  // X(2)
        public string GasExceptionTwiceAllow { get; set; }  // X(1)
        public byte GasHighestDailyCycles { get; set; }  // 9(1)
        public decimal GasExceptHighDayAmount { get; set; }  // S9(09) COMP-3
        public short GasReducedRate { get; set; }  // 9(03)
        public string GasReducedRateCode { get; set; }  // X(1)
        public decimal CondEndingBalance { get; set; }  // S9(09) COMP-3
        public decimal G9InjectionAmount { get; set; }  // S9(09) COMP-3
        public decimal GasCumuCycleStatus { get; set; }  // S9(09) COMP-3
        public decimal GasCumulativeOverage { get; set; }  // S9(09) COMP-3
        public decimal GasOverageTransfer { get; set; }  // S9(09) COMP-3
        public string GasNoPastProductionFlag { get; set; }  // X(1)
        public string GasNoHighestDailyFlag { get; set; }  // X(1)
        public string GasCancelUnderageFlag { get; set; }  // X(1)
        public byte GasPastProductionCycles { get; set; }  // 9(1)
        public decimal GasExceptPastProdAmount { get; set; }  // S9(09) COMP-3
        public decimal LiquidCumuCycleStatus { get; set; }  // S9(09) COMP-3
        public decimal G9PlantLiquid { get; set; }  // S9(09) COMP-3
        public string G9InjectionCreditCode { get; set; }  // X(1)
        public string GasOnShutInListFlag { get; set; }  // X(1) '0'/'1'
        public decimal WellCapability { get; set; }  // S9(09) COMP-3
        public decimal WellMonthlyCapability { get; set; }  // S9(09) COMP-3

        /// <summary>
        /// Returns a naive SQL INSERT statement to insert this record
        /// into the PDGRPTCY table. In production, prefer parameterized queries or ORM.
        /// </summary>
        public string ToSqlInsert()
        {
            return $@"
INSERT INTO dbo.PDGRPTCY (
    RRC_TAPE_RECORD_ID,
    PD_GAS_REPORT_CYCLE_KEY,
    PD_GAS_BALANCING_ALLOW_AMT,
    PD_GAS_BALANCING_ALLOW_CODE,
    PD_GAS_EXCEPTION_TWICE_ALLOW,
    PD_GAS_HIGHEST_DAILY_CYCLES,
    PD_GAS_EXCEPT_HIGH_DAY_AMOUNT,
    PD_GAS_REDUCED_RATE,
    PD_GAS_REDUCED_RATE_CODE,
    PD_COND_ENDING_BALANCE,
    PD_G9_INJECTION_AMOUNT,
    PD_GAS_CUMU_CYCLE_STATUS,
    PD_GAS_CUMULATIVE_OVERAGE,
    PD_GAS_OVERAGE_TRANSFER,
    PD_GAS_NO_PAST_PRODUCTION_FLAG,
    PD_GAS_NO_HIGHEST_DAILY_FLAG,
    PD_GAS_CANCEL_UNDERAGE_FLAG,
    PD_GAS_PAST_PRODUCTION_CYCLES,
    PD_GAS_EXCEPT_PAST_PROD_AMOUNT,
    PD_LIQUID_CUMU_CYCLE_STATUS,
    PD_G9_PLANT_LIQUID,
    PD_G9_INJECTION_CREDIT_CODE,
    PD_GAS_ON_SHUT_IN_LIST_FLAG,
    PD_WELL_CAPABILITY,
    PD_WELL_MONTHLY_CAPABILITY
)
VALUES (
    '14',
    {GasReportCycleKey},
    {GasBalancingAllowAmt},
    '{GasBalancingAllowCode}',
    '{GasExceptionTwiceAllow}',
    {GasHighestDailyCycles},
    {GasExceptHighDayAmount},
    {GasReducedRate},
    '{GasReducedRateCode}',
    {CondEndingBalance},
    {G9InjectionAmount},
    {GasCumuCycleStatus},
    {GasCumulativeOverage},
    {GasOverageTransfer},
    '{GasNoPastProductionFlag}',
    '{GasNoHighestDailyFlag}',
    '{GasCancelUnderageFlag}',
    {GasPastProductionCycles},
    {GasExceptPastProdAmount},
    {LiquidCumuCycleStatus},
    {G9PlantLiquid},
    '{G9InjectionCreditCode}',
    '{GasOnShutInListFlag}',
    {WellCapability},
    {WellMonthlyCapability}
);";
        }
    }

}
