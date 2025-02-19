using Luval.DbConnectionMate;
using Microsoft.Data.SqlClient;
using Microsoft.Extensions.Logging;
using System.Data;
using System.Text;

namespace EBCDIC
{
    static internal class Program
    {
        // For demonstration, we assume each record is 102 bytes (check actual from PDF!)
        private const int RECORD_SIZE = 102;


        static void Main(string[] args)
        {
            var loggerFactory = LoggerFactory.Create(builder =>
            {
                // Add Console logging
                builder.AddConsole();
            });

            Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

            var logger = loggerFactory.CreateLogger("Program");

            var productionProcessor = new ProductionExtractor(logger);

            //Encoding.GetEncoding(37)
            var productionReader = new DatabaseReader(
                new FileInfo(args[0]), 
                Encoding.GetEncoding("IBM037"), RECORD_SIZE, 
                logger);

            productionReader.WhileReadingRecord((recordId, encoding, recordBytes) =>
            {
                productionProcessor.ProcessRecord(recordId, encoding, recordBytes);
            });

            Console.WriteLine("Finished processing file.");
        }
        
    }
}
