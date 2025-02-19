using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EBCDIC.Entities
{
    public class PdocmprdRecord
    {
        // Always "09" for PDOCMPRD
        public decimal PdOilCommProductionAmt { get; set; }

        public string PdOlcoRemovedCmInactvDisc { get; set; }
        public string PdOlcoRemovedNotSpecDisc { get; set; }
        public string PdOlcoRemovedOutOfBalance { get; set; }

        public string PdOilCmFiledByEdiFlag { get; set; }
        public string PdOilCommBatchNumber { get; set; }
        public int PdOilCommItemNumber { get; set; }

        public short PdOilCommPostingYear { get; set; }
        public byte PdOilCommPostingMonth { get; set; }
        public byte PdOilCommPostingDay { get; set; }

        public string PdOlcoRemovedLseSevDisc { get; set; }
        public string PdOlcoRemovedOtherDisc { get; set; }
        public string PdOlcoRemovedLostOilDisc { get; set; }
        public string PdOlcoRemovedCircOilDisc { get; set; }

        public string PdOilCmCorrectedRptFlag { get; set; }
        public string PdOlcoRemovedCode6Disc { get; set; }
        public string PdOlcoRemovedR3Disc { get; set; }
        public string PdOlcoRemovedNetOilDisc { get; set; }

        /// <summary>
        /// Builds a naive SQL INSERT statement for PDOCMPRD.
        /// In production, consider using parameterized queries or an ORM.
        /// </summary>
        public string ToSqlInsert()
        {
            return $@"
INSERT INTO dbo.PDOCMPRD (
    RRC_TAPE_RECORD_ID,
    PD_OIL_COMM_PRODUCTION_AMT,

    PD_OLCO_REMOVED_CM_INACTV_DISC,
    PD_OLCO_REMOVED_NOT_SPEC_DISC,
    PD_OLCO_REMOVED_OUT_OF_BALANCE,

    PD_OIL_CM_FILED_BY_EDI_FLAG,
    PD_OIL_COMM_BATCH_NUMBER,
    PD_OIL_COMM_ITEM_NUMBER,

    PD_OIL_COMM_POSTING_YEAR,
    PD_OIL_COMM_POSTING_MONTH,
    PD_OIL_COMM_POSTING_DAY,

    PD_OLCO_REMOVED_LSE_SEV_DISC,
    PD_OLCO_REMOVED_OTHER_DISC,
    PD_OLCO_REMOVED_LOST_OIL_DISC,
    PD_OLCO_REMOVED_CIRC_OIL_DISC,

    PD_OIL_CM_CORRECTED_RPT_FLAG,
    PD_OLCO_REMOVED_CODE_6_DISC,
    PD_OLCO_REMOVED_R_3_DISC,
    PD_OLCO_REMOVED_NET_OIL_DISC
)
VALUES (
    '09',
    {PdOilCommProductionAmt},

    '{PdOlcoRemovedCmInactvDisc}',
    '{PdOlcoRemovedNotSpecDisc}',
    '{PdOlcoRemovedOutOfBalance}',

    '{PdOilCmFiledByEdiFlag}',
    '{PdOilCommBatchNumber}',
    {PdOilCommItemNumber},

    {PdOilCommPostingYear},
    {PdOilCommPostingMonth},
    {PdOilCommPostingDay},

    '{PdOlcoRemovedLseSevDisc}',
    '{PdOlcoRemovedOtherDisc}',
    '{PdOlcoRemovedLostOilDisc}',
    '{PdOlcoRemovedCircOilDisc}',

    '{PdOilCmCorrectedRptFlag}',
    '{PdOlcoRemovedCode6Disc}',
    '{PdOlcoRemovedR3Disc}',
    '{PdOlcoRemovedNetOilDisc}'
);";
        }
    }

}
