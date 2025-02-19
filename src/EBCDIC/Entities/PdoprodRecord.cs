using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EBCDIC.Entities
{
    public class PdoprodRecord
    {
        public string CorrectedReportFlag { get; set; }      // 'N' or 'Y'
        public decimal ProductionAmount { get; set; }        // from COMP-3
        public decimal CasingheadGasAmount { get; set; }     // from COMP-3
        public decimal CasingheadGasLift { get; set; }       // from COMP-3
        public string BatchNumber { get; set; }              // X(3)
        public int ItemNumber { get; set; }                  // 9(4)
        public short PostingYear { get; set; }               // 9(4)
        public byte PostingMonth { get; set; }               // 9(2)
        public byte PostingDay { get; set; }                 // 9(2)
        public string FiledByEdiFlag { get; set; }           // 'Y' or 'N'

        /// <summary>
        /// Returns a SQL INSERT statement to insert this record
        /// into the PDOPROD table. Real code should prefer parameters/ORM.
        /// </summary>
        public string ToSqlInsert()
        {
            // Hard-code RRC_TAPE_RECORD_ID as '03' since this is PDOPROD.
            // For all string fields, we wrap in quotes.
            // For numeric/decimal fields, we insert them as-is.
            return $@"
INSERT INTO dbo.PDOPROD (
    RRC_TAPE_RECORD_ID,
    PD_OIL_CORRECTED_REPORT_FLAG,
    PD_OIL_PRODUCTION_AMOUNT,
    PD_OIL_CASINGHEAD_GAS_AMOUNT,
    PD_OIL_CASINGHEAD_GAS_LIFT,
    PD_OIL_BATCH_NUMBER,
    PD_OIL_ITEM_NUMBER,
    PD_OIL_POSTING_YEAR,
    PD_OIL_POSTING_MONTH,
    PD_OIL_POSTING_DAY,
    PD_OIL_FILED_BY_EDI_FLAG
)
VALUES (
    '03',
    '{CorrectedReportFlag}',
    {ProductionAmount},
    {CasingheadGasAmount},
    {CasingheadGasLift},
    '{BatchNumber}',
    {ItemNumber},
    {PostingYear},
    {PostingMonth},
    {PostingDay},
    '{FiledByEdiFlag}'
);
";
        }
    }

}
