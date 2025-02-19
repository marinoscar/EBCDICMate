using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EBCDIC.Entities
{
    public class PdodsprkRecord : IDbScriptRecord
    {
        // Always "23" for PDODSPRK
        public short PdOilRmkCentury { get; set; }  // 9(02)
        public short PdOilRmkYear { get; set; }  // 9(02)
        public byte PdOilRmkMonth { get; set; }  // 9(02)
        public byte PdOilRmkDay { get; set; }  // 9(02)
        public string PdOilDispRemarkText { get; set; }  // X(40)

        /// <summary>
        /// Builds a naive SQL INSERT for PDODSPRK. 
        /// In production, prefer parameterized queries or an ORM.
        /// </summary>
        public string ToSqlInsert()
        {
            // We'll replace any single quotes in the remark text to avoid syntax issues.
            string escapedText = PdOilDispRemarkText?.Replace("'", "''") ?? "";

            return $@"
INSERT INTO dbo.PDODSPRK (
    RRC_TAPE_RECORD_ID,
    PD_OIL_RMK_CENTURY,
    PD_OIL_RMK_YEAR,
    PD_OIL_RMK_MONTH,
    PD_OIL_RMK_DAY,
    PD_OIL_DISP_REMARK_TEXT
)
VALUES (
    '23',
    {PdOilRmkCentury},
    {PdOilRmkYear},
    {PdOilRmkMonth},
    {PdOilRmkDay},
    '{escapedText}'
);";
        }
    }

}
