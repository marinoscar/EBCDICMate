using EBCDIC.Entities;
using Luval.DbConnectionMate;
using Microsoft.Data.SqlClient;
using Microsoft.Extensions.Logging;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Data;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EBCDIC
{
    public class DatabaseReader
    {
        private readonly FileInfo _fileInfo;
        private readonly Encoding _encoding;
        private readonly int _recordLength;
        private readonly ILogger _logger;

        public DatabaseReader(FileInfo fileInfo, Encoding encoding, int recordLength, ILogger logger)
        {
            _fileInfo = fileInfo ?? throw new ArgumentNullException(nameof(fileInfo));
            _encoding = encoding ?? throw new ArgumentNullException(nameof(encoding));
            _recordLength = recordLength;
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }

        /// <summary>
        /// Reads records from the file and processes each record using the provided action.
        /// </summary>
        /// <param name="processRecord">Action to process each record. Takes record ID and record bytes as parameters.</param>
        /// <exception cref="FileNotFoundException">Thrown when the file does not exist.</exception>
        /// <exception cref="ArgumentNullException">Thrown when processRecord is null.</exception>
        public void WhileReadingRecord(Func<string, Encoding, byte[], IDbScriptRecord> processRecord)
        {
            if (!_fileInfo.Exists) throw new FileNotFoundException($"File {_fileInfo.FullName} not found.");
            if (processRecord == null) throw new ArgumentNullException(nameof(processRecord));
            var rowCount = 0;
            var scriptCount = 0;
            var sw = new Stopwatch();
            var sb = new StringBuilder();
            sw.Start();
            try
            {
                using (var connection = new SqlConnection(GetConnectionString()))
                {
                    connection.Open();
                    using (FileStream fs = new FileStream(_fileInfo.FullName, FileMode.Open, FileAccess.Read))
                    {
                        using (BinaryReader br = new BinaryReader(fs))
                        {
                            while (br.BaseStream.Position < br.BaseStream.Length)
                            {
                                // Make sure we don't read past the end if the file is shorter
                                if (br.BaseStream.Length - br.BaseStream.Position < _recordLength)
                                {
                                    _logger.LogWarning("Incomplete record at end of file.");
                                    break;
                                }

                                byte[] recordBytes = br.ReadBytes(_recordLength);

                                string recordId = _encoding.GetString(recordBytes, 0, 2);

                                var recordScript = processRecord(recordId, _encoding, recordBytes);
                                sb.AppendLine(recordScript.ToSqlInsert());
                                rowCount++;
                                scriptCount++;
                                if(scriptCount > 100)
                                {
                                    RunSql(sb.ToString(), connection);
                                    scriptCount = 0;
                                    sb.Clear();
                                }
                            }
                        }
                    }
                }
                sw.Stop();
                _logger.LogInformation($"Processed {rowCount} records.");
                _logger.LogInformation($"Elapsed time: {sw.Elapsed}");
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "An error occurred while reading the file.");
                throw;
            }
        }

        private void RunSql(string sql, SqlConnection connection)
        {
            using (var command = new SqlCommand(sql, connection))
            {
                try
                {
                    _logger.LogDebug(sql);
                    command.ExecuteNonQuery();
                }
                catch (Exception)
                {
                    _logger.LogError("Failed to run:\n" + sql + "\n");
                    throw;
                }
            }
        }

        private static string GetConnectionString()
        {
            return "Server=.\\SQLEXPRESS;Database=EBCDIC;Integrated Security=True;TrustServerCertificate=True";
        }
    }
}
