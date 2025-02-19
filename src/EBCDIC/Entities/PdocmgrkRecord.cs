using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EBCDIC.Entities
{
    public class PdocmgrkRecord : IDbScriptRecord
    {
        // Always "24" for PDOCMGRK
        public short PdOilCmgRmkCentury { get; set; } // 9(02)
        public short PdOilCmgRmkYear { get; set; } // 9(02)
        public byte PdOilCmgRmkMonth { get; set; } // 9(02)
        public byte PdOilCmgRmkDay { get; set; } // 9(02)
        public string PdOilCmgRemarkText { get; set; } // X(40)

        /// <summary>
        /// Builds a naive SQL INSERT for PDOCMGRK.
        /// In production code, use parameterized queries or ORM for safety.
        /// </summary>
        public string ToSqlInsert()
        {
            // Escape any single quotes in the remark text to avoid SQL errors
            string escapedText = PdOilCmgRemarkText?.Replace("'", "''") ?? "";

            return $@"
INSERT INTO dbo.PDOCMGRK (
    RRC_TAPE_RECORD_ID,
    PD_OIL_CMG_RMK_CENTURY,
    PD_OIL_CMG_RMK_YEAR,
    PD_OIL_CMG_RMK_MONTH,
    PD_OIL_CMG_RMK_DAY,
    PD_OIL_CMG_REMARK_TEXT
)
VALUES (
    '24',
    {PdOilCmgRmkCentury},
    {PdOilCmgRmkYear},
    {PdOilCmgRmkMonth},
    {PdOilCmgRmkDay},
    '{escapedText}'
);";
        }
    }

}
