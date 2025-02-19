using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EBCDIC.Entities
{
    public class PdodspRecord : IDbScriptRecord
    {
        // Record ID always "05"
        public short PdOilDispositionCode { get; set; }    // PIC 9(2)
        public decimal PdOilDispositionAmount { get; set; } // S9(09) COMP-3

        /// <summary>
        /// Returns a naive SQL INSERT statement for PDODSP.
        /// In production, use parameterized queries or ORM to avoid injection.
        /// </summary>
        public string ToSqlInsert()
        {
            return $@"
INSERT INTO dbo.PDODSP (
    RRC_TAPE_RECORD_ID,
    PD_OIL_DISPOSITION_CODE,
    PD_OIL_DISPOSITION_AMOUNT
)
VALUES (
    '05',
    {PdOilDispositionCode},
    {PdOilDispositionAmount}
);";
        }
    }

}
