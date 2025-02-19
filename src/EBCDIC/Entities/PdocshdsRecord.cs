using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EBCDIC.Entities
{
    public class PdocshdsRecord : IDbScriptRecord
    {
        // Record ID always "06" for PDOCSHDS
        public short PdCshDispositionCode { get; set; }    // PIC 9(2)
        public decimal PdCshDispositionAmt { get; set; }   // S9(09) COMP-3

        /// <summary>
        /// Returns a naive SQL INSERT statement for PDOCSHDS.
        /// In production, use parameterized queries or ORM for safety.
        /// </summary>
        public string ToSqlInsert()
        {
            return $@"
INSERT INTO dbo.PDOCSHDS (
    RRC_TAPE_RECORD_ID,
    PD_CSH_DISPOSITION_CODE,
    PD_CSH_DISPOSITION_AMT
)
VALUES (
    '06',
    {PdCshDispositionCode},
    {PdCshDispositionAmt}
);";
        }
    }

}
