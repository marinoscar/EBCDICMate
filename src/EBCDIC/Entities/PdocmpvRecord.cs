using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EBCDIC.Entities
{
    public class PdocmpvRecord
    {
        // Always "12" for PDOCMPV
        public short PdOilCommPrevPostingYear { get; set; } // PIC 9(04)
        public byte PdOilCommPrevPostingMonth { get; set; } // PIC 9(02)
        public byte PdOilCommPrevPostingDay { get; set; } // PIC 9(02)

        public string PdOilCommPrevBatchNumber { get; set; } // X(03)
        public int PdOilCommPrevItemNumber { get; set; } // 9(04)
        public string PdOilCommPrevChangedFlag { get; set; } // 'C' or space
        public string PdOilCommPrevEdiFlag { get; set; } // 'Y' or space

        /// <summary>
        /// Returns a naive SQL INSERT statement for PDOCMPV.
        /// In production, prefer parameterized queries or an ORM for safety.
        /// </summary>
        public string ToSqlInsert()
        {
            return $@"
INSERT INTO dbo.PDOCMPV (
    RRC_TAPE_RECORD_ID,
    PD_OIL_COMM_PREV_POSTING_YEAR,
    PD_OIL_COMM_PREV_POSTING_MONTH,
    PD_OIL_COMM_PREV_POSTING_DAY,
    PD_OIL_COMM_PREV_BATCH_NUMBER,
    PD_OIL_COMM_PREV_ITEM_NUMBER,
    PD_OIL_COMM_PREV_CHANGED_FLAG,
    PD_OIL_COMM_PREV_EDI_FLAG
)
VALUES (
    '12',
    {PdOilCommPrevPostingYear},
    {PdOilCommPrevPostingMonth},
    {PdOilCommPrevPostingDay},
    '{PdOilCommPrevBatchNumber}',
    {PdOilCommPrevItemNumber},
    '{PdOilCommPrevChangedFlag}',
    '{PdOilCommPrevEdiFlag}'
);";
        }
    }

}
