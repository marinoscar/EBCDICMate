using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EBCDIC.Entities
{
    public class PdgprodRecord : IDbScriptRecord
    {
        // Record ID always "15" for this segment
        // Gas Production Data
        public decimal GasProd { get; set; }              // PD-GAS-PROD (S9(09) COMP-3)
        public decimal GasLiftGasInjected { get; set; }   // PD-GAS-LIFT-GAS-INJECTED (S9(09) COMP-3)
        public decimal CondProd { get; set; }             // PD-COND-PROD (S9(09) COMP-3)
        public string GasBatchNumber { get; set; }        // PIC X(3)
        public int GasItemNumber { get; set; }            // PIC 9(4)
        public short GasPostingYear { get; set; }         // PIC 9(4)
        public byte GasPostingMonth { get; set; }         // PIC 9(2)
        public byte GasPostingDay { get; set; }           // PIC 9(2)

        // Discrepancies & Flags
        public string RemovedFlaringDiscrep { get; set; }     // offset 33
        public string RemovedSealedDiscrep { get; set; }      // offset 34
        public string RemovedH8Discrep { get; set; }          // offset 35
        public string RemovedCircOilDiscrep { get; set; }     // offset 36
        public string RemovedOtherDiscrep { get; set; }       // offset 37
        public string RemovedCode4Discrep { get; set; }       // offset 38
        public string GasCorrectedReportFlag { get; set; }    // offset 39
        public string RemovedCode6Discrep { get; set; }       // offset 40
        public string GasFiledByEdiFlag { get; set; }         // offset 41
        public string RemovedWellProdDiscrep { get; set; }    // offset 42
        public string RemovedConExtLDiscrep { get; set; }     // offset 43
        public string RemovedConInsufDiscrep { get; set; }    // offset 44
        public string RemovedCode3Discrep { get; set; }       // offset 45

        /// <summary>
        /// Returns a naive SQL INSERT statement to insert this record
        /// into the [PDGPROD] table. In production, prefer parameterized queries / ORM.
        /// </summary>
        public string ToSqlInsert()
        {
            return $@"
INSERT INTO dbo.PDGPROD (
    RRC_TAPE_RECORD_ID,
    PD_GAS_PROD,
    PD_GAS_LIFT_GAS_INJECTED,
    PD_COND_PROD,
    PD_GAS_BATCH_NUMBER,
    PD_GAS_ITEM_NUMBER,
    PD_GAS_POSTING_YEAR,
    PD_GAS_POSTING_MONTH,
    PD_GAS_POSTING_DAY,
    PD_G_REMOVED_FLARING_DISCREP,
    PD_G_REMOVED_SEALED_DISCREP,
    PD_G_REMOVED_H_8_DISCREP,
    PD_G_REMOVED_CIRC_OIL_DISCREP,
    PD_G_REMOVED_OTHER_DISCREP,
    PD_G_REMOVED_CODE4_DISCREP,
    PD_GAS_CORRECTED_REPORT_FLAG,
    PD_G_REMOVED_CODE_6_DISCREP,
    PD_GAS_FILED_BY_EDI_FLAG,
    PD_G_REMOVED_WELL_PROD_DISCREP,
    PD_G_REMOVED_CON_EXT_L_DISCREP,
    PD_G_REMOVED_CON_INSUF_DISCREP,
    PD_G_REMOVED_CODE_3_DISCREP
)
VALUES (
    '15',  -- RRC_TAPE_RECORD_ID
    {GasProd},
    {GasLiftGasInjected},
    {CondProd},
    '{GasBatchNumber}',
    {GasItemNumber},
    {GasPostingYear},
    {GasPostingMonth},
    {GasPostingDay},
    '{RemovedFlaringDiscrep}',
    '{RemovedSealedDiscrep}',
    '{RemovedH8Discrep}',
    '{RemovedCircOilDiscrep}',
    '{RemovedOtherDiscrep}',
    '{RemovedCode4Discrep}',
    '{GasCorrectedReportFlag}',
    '{RemovedCode6Discrep}',
    '{GasFiledByEdiFlag}',
    '{RemovedWellProdDiscrep}',
    '{RemovedConExtLDiscrep}',
    '{RemovedConInsufDiscrep}',
    '{RemovedCode3Discrep}'
);";
        }
    }

}
