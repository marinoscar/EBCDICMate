using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EBCDIC.Entities
{
    public class PdrootOilRecord
    {
        // Record ID is always "01" for PDROOT_OIL.
        public string PdOilCode { get; set; }               // CHAR(1)  e.g. "O"
        public short PdOilDistrict { get; set; }            // SMALLINT
        public int PdOilLeaseNbr { get; set; }              // INT
        public decimal PdMovableBalance { get; set; }       // DECIMAL(9,0)
        public decimal PdBeginningOilStatus { get; set; }   // DECIMAL(9,0)
        public decimal PdBeginningCsghdStatus { get; set; } // DECIMAL(9,0)
        public decimal PdOilOldestEomBalance { get; set; }  // DECIMAL(9,0)

        /// <summary>
        /// Creates a naive SQL INSERT statement for the PDROOT_OIL table.
        /// In production code, consider using parameterized queries or an ORM.
        /// </summary>
        public string ToSqlInsert()
        {
            return $@"
INSERT INTO dbo.PDROOT_OIL (
    RRC_TAPE_RECORD_ID,
    PD_OIL_CODE,
    PD_OIL_DISTRICT,
    PD_OIL_LEASE_NBR,
    PD_MOVABLE_BALANCE,
    PD_BEGINNING_OIL_STATUS,
    PD_BEGINNING_CSGHD_STATUS,
    PD_OIL_OLDEST_EOM_BALANCE
)
VALUES (
    '01',
        '{PdOilCode}',
    { PdOilDistrict},
    { PdOilLeaseNbr},
    { PdMovableBalance},
    { PdBeginningOilStatus},
    { PdBeginningCsghdStatus},
    { PdOilOldestEomBalance}
); ";
        }
    }

}
