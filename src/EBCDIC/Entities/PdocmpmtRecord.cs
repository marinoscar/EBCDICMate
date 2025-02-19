using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EBCDIC.Entities
{
    public class PdocmpmtRecord : IDbScriptRecord
    {
        // Record ID always "08" (Commingle Permit Segment)
        public short PdOilCommingleDistrict { get; set; }    // 9(2)
        public int PdOilCommingleNumber { get; set; }    // 9(5)
        public decimal PdOilCommingleEndBalance { get; set; } // S9(09) COMP-3

        /// <summary>
        /// Returns a naive SQL INSERT statement for PDOCMPMT.
        /// In production, consider parameterized queries or an ORM.
        /// </summary>
        public string ToSqlInsert()
        {
            return $@"
INSERT INTO dbo.PDOCMPMT (
    RRC_TAPE_RECORD_ID,
    PD_OIL_COMMINGLE_DISTRICT,
    PD_OIL_COMMINGLE_NUMBER,
    PD_OIL_COMMINGLE_END_BALANCE
)
VALUES (
    '08',
    {PdOilCommingleDistrict},
    {PdOilCommingleNumber},
    {PdOilCommingleEndBalance}
);";
        }
    }

}
