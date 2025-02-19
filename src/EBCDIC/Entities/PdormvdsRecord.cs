using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EBCDIC.Entities
{
    public class PdormvdsRecord : IDbScriptRecord
    {
        // Always "04" for PDORMVDS
        public string PdORemovedCode6Discrep { get; set; }  // offset 2
        public string PdORemovedR3Discrep { get; set; }  // offset 3
        public string PdORemovedNetOilDiscrep { get; set; }  // offset 4
        public string PdORemovedLseSevDiscrep { get; set; }  // offset 5
        public string PdORemovedOtherDiscrep { get; set; }  // offset 6
        public string PdORemovedLostOilDiscrep { get; set; }  // offset 7
        public string PdORemovedCircOilDiscrep { get; set; }  // offset 8
        public string PdORemovedPrdInactDiscrep { get; set; }  // offset 9
        public string PdORemovedFileCommDiscrep { get; set; }  // offset 10

        /// <summary>
        /// Returns a naive SQL INSERT statement for PDORMVDS.
        /// In production code, use parameterized queries or an ORM.
        /// </summary>
        public string ToSqlInsert()
        {
            return $@"
INSERT INTO dbo.PDORMVDS (
    RRC_TAPE_RECORD_ID,
    PD_O_REMOVED_CODE_6_DISCREP,
    PD_O_REMOVED_R_3_DISCREP,
    PD_O_REMOVED_NET_OIL_DISCREP,
    PD_O_REMOVED_LSE_SEV_DISCREP,
    PD_O_REMOVED_OTHER_DISCREP,
    PD_O_REMOVED_LOST_OIL_DISCREP,
    PD_O_REMOVED_CIRC_OIL_DISCREP,
    PD_O_REMOVED_PRD_INACT_DISCREP,
    PD_O_REMOVED_FILE_COMM_DISCREP
)
VALUES (
    '04',
    '{PdORemovedCode6Discrep}',
    '{PdORemovedR3Discrep}',
    '{PdORemovedNetOilDiscrep}',
    '{PdORemovedLseSevDiscrep}',
    '{PdORemovedOtherDiscrep}',
    '{PdORemovedLostOilDiscrep}',
    '{PdORemovedCircOilDiscrep}',
    '{PdORemovedPrdInactDiscrep}',
    '{PdORemovedFileCommDiscrep}'
);";
        }
    }

}
