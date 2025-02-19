using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EBCDIC.Entities
{
    public class PdooebRecord
    {
        // Always "11" for PDOOEB
        public decimal PdCmOldestEomBal { get; set; } // S9(09) COMP-3

        /// <summary>
        /// Returns a naive SQL INSERT statement for PDOOEB.
        /// In production, prefer parameterized queries or ORM for security.
        /// </summary>
        public string ToSqlInsert()
        {
            return $@"
INSERT INTO dbo.PDOOEB (
    RRC_TAPE_RECORD_ID,
    PD_CM_OLDEST_EOM_BAL
)
VALUES (
    '11',
    {PdCmOldestEomBal}
);";
        }
    }

}
