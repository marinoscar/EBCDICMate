using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EBCDIC.Entities
{
    public class PdocmodsRecord : IDbScriptRecord
    {
        // Record ID always "10"
        public short PdOilOlcoDispositionCode { get; set; }   // PIC 9(02)
        public decimal PdOilOlcoDispositionAmt { get; set; }  // S9(09) COMP-3

        /// <summary>
        /// Returns a naive SQL INSERT statement for PDOCMODS.
        /// In production, use parameterized queries or an ORM to avoid injection.
        /// </summary>
        public string ToSqlInsert()
        {
            return $@"
INSERT INTO dbo.PDOCMODS (
    RRC_TAPE_RECORD_ID,
    PD_OIL_OLCO_DISPOSITION_CODE,
    PD_OIL_OLCO_DISPOSITION_AMT
)
VALUES (
    '10',
    {PdOilOlcoDispositionCode},
    {PdOilOlcoDispositionAmt}
);";
        }
    }

}
