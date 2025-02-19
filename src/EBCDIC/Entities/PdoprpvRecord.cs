using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EBCDIC.Entities
{
    public class PdoprpvRecord : IDbScriptRecord
    {
        // Always "07" for PDOPRPV
        public short PdOilPrevPostingYear { get; set; } // PIC 9(4)
        public byte PdOilPrevPostingMonth { get; set; } // PIC 9(2)
        public byte PdOilPrevPostingDay { get; set; } // PIC 9(2)
        public string PdOilPrevBatchNumber { get; set; } // X(3)
        public int PdOilPrevItemNumber { get; set; } // 9(4)
        public string PdOilPrevChangedFlag { get; set; } // 'C' or space
        public string PdOilPrevFiledByEdiFlag { get; set; } // 'Y' or space

        /// <summary>
        /// Generates a naive SQL INSERT statement for PDOPRPV.
        /// In production, strongly consider parameterized queries or an ORM.
        /// </summary>
        public string ToSqlInsert()
        {
            return $@"
INSERT INTO dbo.PDOPRPV (
    RRC_TAPE_RECORD_ID,
    PD_OIL_PREV_POSTING_YEAR,
    PD_OIL_PREV_POSTING_MONTH,
    PD_OIL_PREV_POSTING_DAY,
    PD_OIL_PREV_BATCH_NUMBER,
    PD_OIL_PREV_ITEM_NUMBER,
    PD_OIL_PREV_CHANGED_FLAG,
    PD_OIL_PREV_FILED_BY_EDI_FLAG
)
VALUES (
    '07',
    {PdOilPrevPostingYear},
    {PdOilPrevPostingMonth},
    {PdOilPrevPostingDay},
    '{PdOilPrevBatchNumber}',
    {PdOilPrevItemNumber},
    '{PdOilPrevChangedFlag}',
    '{PdOilPrevFiledByEdiFlag}'
);";
        }
    }

}
