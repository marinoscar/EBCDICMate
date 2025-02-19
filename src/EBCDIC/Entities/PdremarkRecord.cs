using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EBCDIC.Entities
{
    public class PdremarkRecord
    {
        // Always "22" for PDREMARK
        public int PdRemarkNumber { get; set; }  // PIC 9(3)
        public int PdRemarkLineNo { get; set; }  // PIC 9(2)
        public string PdRemarkText { get; set; }  // PIC X(70)

        /// <summary>
        /// Returns a naive SQL INSERT for PDREMARK.
        /// In production, consider parameterized queries or ORM to avoid injection.
        /// </summary>
        public string ToSqlInsert()
        {
            // We'll wrap string values in quotes and numeric as-is.
            return $@"
INSERT INTO dbo.PDREMARK (
    RRC_TAPE_RECORD_ID,
    PD_REMARK_NUMBER,
    PD_REMARK_LINE_NO,
    PD_REMARK_TEXT
)
VALUES (
    '22',
    {PdRemarkNumber},
    {PdRemarkLineNo},
    '{PdRemarkText?.Replace("'", "''")}'
);";
        }
    }

}
