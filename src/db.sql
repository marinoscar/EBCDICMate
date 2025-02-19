USE [master]
GO
/****** Object:  Database [EBCDIC]    Script Date: 2/19/2025 2:46:37 PM ******/
CREATE DATABASE [EBCDIC]
 CONTAINMENT = NONE
 ON  PRIMARY 
( NAME = N'EBCDIC', FILENAME = N'C:\Program Files\Microsoft SQL Server\MSSQL16.SQLEXPRESS\MSSQL\DATA\EBCDIC.mdf' , SIZE = 8192KB , MAXSIZE = UNLIMITED, FILEGROWTH = 65536KB )
 LOG ON 
( NAME = N'EBCDIC_log', FILENAME = N'C:\Program Files\Microsoft SQL Server\MSSQL16.SQLEXPRESS\MSSQL\DATA\EBCDIC_log.ldf' , SIZE = 8192KB , MAXSIZE = 2048GB , FILEGROWTH = 65536KB )
 WITH CATALOG_COLLATION = DATABASE_DEFAULT, LEDGER = OFF
GO
ALTER DATABASE [EBCDIC] SET COMPATIBILITY_LEVEL = 160
GO
IF (1 = FULLTEXTSERVICEPROPERTY('IsFullTextInstalled'))
begin
EXEC [EBCDIC].[dbo].[sp_fulltext_database] @action = 'enable'
end
GO
ALTER DATABASE [EBCDIC] SET ANSI_NULL_DEFAULT OFF 
GO
ALTER DATABASE [EBCDIC] SET ANSI_NULLS OFF 
GO
ALTER DATABASE [EBCDIC] SET ANSI_PADDING OFF 
GO
ALTER DATABASE [EBCDIC] SET ANSI_WARNINGS OFF 
GO
ALTER DATABASE [EBCDIC] SET ARITHABORT OFF 
GO
ALTER DATABASE [EBCDIC] SET AUTO_CLOSE OFF 
GO
ALTER DATABASE [EBCDIC] SET AUTO_SHRINK OFF 
GO
ALTER DATABASE [EBCDIC] SET AUTO_UPDATE_STATISTICS ON 
GO
ALTER DATABASE [EBCDIC] SET CURSOR_CLOSE_ON_COMMIT OFF 
GO
ALTER DATABASE [EBCDIC] SET CURSOR_DEFAULT  GLOBAL 
GO
ALTER DATABASE [EBCDIC] SET CONCAT_NULL_YIELDS_NULL OFF 
GO
ALTER DATABASE [EBCDIC] SET NUMERIC_ROUNDABORT OFF 
GO
ALTER DATABASE [EBCDIC] SET QUOTED_IDENTIFIER OFF 
GO
ALTER DATABASE [EBCDIC] SET RECURSIVE_TRIGGERS OFF 
GO
ALTER DATABASE [EBCDIC] SET  DISABLE_BROKER 
GO
ALTER DATABASE [EBCDIC] SET AUTO_UPDATE_STATISTICS_ASYNC OFF 
GO
ALTER DATABASE [EBCDIC] SET DATE_CORRELATION_OPTIMIZATION OFF 
GO
ALTER DATABASE [EBCDIC] SET TRUSTWORTHY OFF 
GO
ALTER DATABASE [EBCDIC] SET ALLOW_SNAPSHOT_ISOLATION OFF 
GO
ALTER DATABASE [EBCDIC] SET PARAMETERIZATION SIMPLE 
GO
ALTER DATABASE [EBCDIC] SET READ_COMMITTED_SNAPSHOT OFF 
GO
ALTER DATABASE [EBCDIC] SET HONOR_BROKER_PRIORITY OFF 
GO
ALTER DATABASE [EBCDIC] SET RECOVERY SIMPLE 
GO
ALTER DATABASE [EBCDIC] SET  MULTI_USER 
GO
ALTER DATABASE [EBCDIC] SET PAGE_VERIFY CHECKSUM  
GO
ALTER DATABASE [EBCDIC] SET DB_CHAINING OFF 
GO
ALTER DATABASE [EBCDIC] SET FILESTREAM( NON_TRANSACTED_ACCESS = OFF ) 
GO
ALTER DATABASE [EBCDIC] SET TARGET_RECOVERY_TIME = 60 SECONDS 
GO
ALTER DATABASE [EBCDIC] SET DELAYED_DURABILITY = DISABLED 
GO
ALTER DATABASE [EBCDIC] SET ACCELERATED_DATABASE_RECOVERY = OFF  
GO
ALTER DATABASE [EBCDIC] SET QUERY_STORE = ON
GO
ALTER DATABASE [EBCDIC] SET QUERY_STORE (OPERATION_MODE = READ_WRITE, CLEANUP_POLICY = (STALE_QUERY_THRESHOLD_DAYS = 30), DATA_FLUSH_INTERVAL_SECONDS = 900, INTERVAL_LENGTH_MINUTES = 60, MAX_STORAGE_SIZE_MB = 1000, QUERY_CAPTURE_MODE = AUTO, SIZE_BASED_CLEANUP_MODE = AUTO, MAX_PLANS_PER_QUERY = 200, WAIT_STATS_CAPTURE_MODE = ON)
GO
USE [EBCDIC]
GO
/****** Object:  Table [dbo].[PDGPROD]    Script Date: 2/19/2025 2:46:37 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PDGPROD](
	[RRC_TAPE_RECORD_ID] [char](2) NOT NULL,
	[PD_GAS_PROD] [decimal](9, 0) NULL,
	[PD_GAS_LIFT_GAS_INJECTED] [decimal](9, 0) NULL,
	[PD_COND_PROD] [decimal](9, 0) NULL,
	[PD_GAS_BATCH_NUMBER] [char](3) NULL,
	[PD_GAS_ITEM_NUMBER] [int] NULL,
	[PD_GAS_POSTING_YEAR] [smallint] NULL,
	[PD_GAS_POSTING_MONTH] [tinyint] NULL,
	[PD_GAS_POSTING_DAY] [tinyint] NULL,
	[PD_G_REMOVED_FLARING_DISCREP] [char](1) NULL,
	[PD_G_REMOVED_SEALED_DISCREP] [char](1) NULL,
	[PD_G_REMOVED_H_8_DISCREP] [char](1) NULL,
	[PD_G_REMOVED_CIRC_OIL_DISCREP] [char](1) NULL,
	[PD_G_REMOVED_OTHER_DISCREP] [char](1) NULL,
	[PD_G_REMOVED_CODE4_DISCREP] [char](1) NULL,
	[PD_GAS_CORRECTED_REPORT_FLAG] [char](1) NOT NULL,
	[PD_G_REMOVED_CODE_6_DISCREP] [char](1) NULL,
	[PD_GAS_FILED_BY_EDI_FLAG] [char](1) NULL,
	[PD_G_REMOVED_WELL_PROD_DISCREP] [char](1) NULL,
	[PD_G_REMOVED_CON_EXT_L_DISCREP] [char](1) NULL,
	[PD_G_REMOVED_CON_INSUF_DISCREP] [char](1) NULL,
	[PD_G_REMOVED_CODE_3_DISCREP] [char](1) NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[PDGRPTCY]    Script Date: 2/19/2025 2:46:37 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PDGRPTCY](
	[RRC_TAPE_RECORD_ID] [char](2) NOT NULL,
	[PD_GAS_REPORT_CYCLE_KEY] [int] NOT NULL,
	[PD_GAS_BALANCING_ALLOW_AMT] [decimal](9, 0) NULL,
	[PD_GAS_BALANCING_ALLOW_CODE] [char](2) NULL,
	[PD_GAS_EXCEPTION_TWICE_ALLOW] [char](1) NULL,
	[PD_GAS_HIGHEST_DAILY_CYCLES] [tinyint] NULL,
	[PD_GAS_EXCEPT_HIGH_DAY_AMOUNT] [decimal](9, 0) NULL,
	[PD_GAS_REDUCED_RATE] [smallint] NULL,
	[PD_GAS_REDUCED_RATE_CODE] [char](1) NULL,
	[PD_COND_ENDING_BALANCE] [decimal](9, 0) NULL,
	[PD_G9_INJECTION_AMOUNT] [decimal](9, 0) NULL,
	[PD_GAS_CUMU_CYCLE_STATUS] [decimal](9, 0) NULL,
	[PD_GAS_CUMULATIVE_OVERAGE] [decimal](9, 0) NULL,
	[PD_GAS_OVERAGE_TRANSFER] [decimal](9, 0) NULL,
	[PD_GAS_NO_PAST_PRODUCTION_FLAG] [char](1) NULL,
	[PD_GAS_NO_HIGHEST_DAILY_FLAG] [char](1) NULL,
	[PD_GAS_CANCEL_UNDERAGE_FLAG] [char](1) NULL,
	[PD_GAS_PAST_PRODUCTION_CYCLES] [tinyint] NULL,
	[PD_GAS_EXCEPT_PAST_PROD_AMOUNT] [decimal](9, 0) NULL,
	[PD_LIQUID_CUMU_CYCLE_STATUS] [decimal](9, 0) NULL,
	[PD_G9_PLANT_LIQUID] [decimal](9, 0) NULL,
	[PD_G9_INJECTION_CREDIT_CODE] [char](1) NULL,
	[PD_GAS_ON_SHUT_IN_LIST_FLAG] [char](1) NULL,
	[PD_WELL_CAPABILITY] [decimal](9, 0) NULL,
	[PD_WELL_MONTHLY_CAPABILITY] [decimal](9, 0) NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[PDOCMGRK]    Script Date: 2/19/2025 2:46:37 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PDOCMGRK](
	[RRC_TAPE_RECORD_ID] [char](2) NOT NULL,
	[PD_OIL_CMG_RMK_CENTURY] [smallint] NULL,
	[PD_OIL_CMG_RMK_YEAR] [smallint] NULL,
	[PD_OIL_CMG_RMK_MONTH] [tinyint] NULL,
	[PD_OIL_CMG_RMK_DAY] [tinyint] NULL,
	[PD_OIL_CMG_REMARK_TEXT] [varchar](40) NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[PDOCMODS]    Script Date: 2/19/2025 2:46:37 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PDOCMODS](
	[RRC_TAPE_RECORD_ID] [char](2) NOT NULL,
	[PD_OIL_OLCO_DISPOSITION_CODE] [smallint] NOT NULL,
	[PD_OIL_OLCO_DISPOSITION_AMT] [decimal](9, 0) NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[PDOCMPMT]    Script Date: 2/19/2025 2:46:37 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PDOCMPMT](
	[RRC_TAPE_RECORD_ID] [char](2) NOT NULL,
	[PD_OIL_COMMINGLE_DISTRICT] [smallint] NOT NULL,
	[PD_OIL_COMMINGLE_NUMBER] [int] NOT NULL,
	[PD_OIL_COMMINGLE_END_BALANCE] [decimal](9, 0) NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[PDOCMPRD]    Script Date: 2/19/2025 2:46:37 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PDOCMPRD](
	[RRC_TAPE_RECORD_ID] [char](2) NOT NULL,
	[PD_OIL_COMM_PRODUCTION_AMT] [decimal](9, 0) NULL,
	[PD_OLCO_REMOVED_CM_INACTV_DISC] [char](1) NOT NULL,
	[PD_OLCO_REMOVED_NOT_SPEC_DISC] [char](1) NOT NULL,
	[PD_OLCO_REMOVED_OUT_OF_BALANCE] [char](1) NOT NULL,
	[PD_OIL_CM_FILED_BY_EDI_FLAG] [char](1) NOT NULL,
	[PD_OIL_COMM_BATCH_NUMBER] [char](3) NULL,
	[PD_OIL_COMM_ITEM_NUMBER] [int] NULL,
	[PD_OIL_COMM_POSTING_YEAR] [smallint] NULL,
	[PD_OIL_COMM_POSTING_MONTH] [tinyint] NULL,
	[PD_OIL_COMM_POSTING_DAY] [tinyint] NULL,
	[PD_OLCO_REMOVED_LSE_SEV_DISC] [char](1) NOT NULL,
	[PD_OLCO_REMOVED_OTHER_DISC] [char](1) NOT NULL,
	[PD_OLCO_REMOVED_LOST_OIL_DISC] [char](1) NOT NULL,
	[PD_OLCO_REMOVED_CIRC_OIL_DISC] [char](1) NOT NULL,
	[PD_OIL_CM_CORRECTED_RPT_FLAG] [char](1) NOT NULL,
	[PD_OLCO_REMOVED_CODE_6_DISC] [char](1) NOT NULL,
	[PD_OLCO_REMOVED_R_3_DISC] [char](1) NOT NULL,
	[PD_OLCO_REMOVED_NET_OIL_DISC] [char](1) NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[PDOCMPV]    Script Date: 2/19/2025 2:46:37 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PDOCMPV](
	[RRC_TAPE_RECORD_ID] [char](2) NOT NULL,
	[PD_OIL_COMM_PREV_POSTING_YEAR] [smallint] NULL,
	[PD_OIL_COMM_PREV_POSTING_MONTH] [tinyint] NULL,
	[PD_OIL_COMM_PREV_POSTING_DAY] [tinyint] NULL,
	[PD_OIL_COMM_PREV_BATCH_NUMBER] [char](3) NULL,
	[PD_OIL_COMM_PREV_ITEM_NUMBER] [int] NULL,
	[PD_OIL_COMM_PREV_CHANGED_FLAG] [char](1) NULL,
	[PD_OIL_COMM_PREV_EDI_FLAG] [char](1) NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[PDOCSHDS]    Script Date: 2/19/2025 2:46:37 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PDOCSHDS](
	[RRC_TAPE_RECORD_ID] [char](2) NOT NULL,
	[PD_CSH_DISPOSITION_CODE] [smallint] NOT NULL,
	[PD_CSH_DISPOSITION_AMT] [decimal](9, 0) NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[PDODSP]    Script Date: 2/19/2025 2:46:37 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PDODSP](
	[RRC_TAPE_RECORD_ID] [char](2) NOT NULL,
	[PD_OIL_DISPOSITION_CODE] [smallint] NOT NULL,
	[PD_OIL_DISPOSITION_AMOUNT] [decimal](9, 0) NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[PDODSPRK]    Script Date: 2/19/2025 2:46:37 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PDODSPRK](
	[RRC_TAPE_RECORD_ID] [char](2) NOT NULL,
	[PD_OIL_RMK_CENTURY] [smallint] NULL,
	[PD_OIL_RMK_YEAR] [smallint] NULL,
	[PD_OIL_RMK_MONTH] [tinyint] NULL,
	[PD_OIL_RMK_DAY] [tinyint] NULL,
	[PD_OIL_DISP_REMARK_TEXT] [varchar](40) NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[PDOOEB]    Script Date: 2/19/2025 2:46:37 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PDOOEB](
	[RRC_TAPE_RECORD_ID] [char](2) NOT NULL,
	[PD_CM_OLDEST_EOM_BAL] [decimal](9, 0) NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[PDOPROD]    Script Date: 2/19/2025 2:46:37 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PDOPROD](
	[RRC_TAPE_RECORD_ID] [char](2) NOT NULL,
	[PD_OIL_CORRECTED_REPORT_FLAG] [char](1) NOT NULL,
	[PD_OIL_PRODUCTION_AMOUNT] [decimal](9, 0) NULL,
	[PD_OIL_CASINGHEAD_GAS_AMOUNT] [decimal](9, 0) NULL,
	[PD_OIL_CASINGHEAD_GAS_LIFT] [decimal](9, 0) NULL,
	[PD_OIL_BATCH_NUMBER] [char](3) NULL,
	[PD_OIL_ITEM_NUMBER] [int] NULL,
	[PD_OIL_POSTING_YEAR] [smallint] NULL,
	[PD_OIL_POSTING_MONTH] [tinyint] NULL,
	[PD_OIL_POSTING_DAY] [tinyint] NULL,
	[PD_OIL_FILED_BY_EDI_FLAG] [char](1) NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[PDOPRPV]    Script Date: 2/19/2025 2:46:37 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PDOPRPV](
	[RRC_TAPE_RECORD_ID] [char](2) NOT NULL,
	[PD_OIL_PREV_POSTING_YEAR] [smallint] NULL,
	[PD_OIL_PREV_POSTING_MONTH] [tinyint] NULL,
	[PD_OIL_PREV_POSTING_DAY] [tinyint] NULL,
	[PD_OIL_PREV_BATCH_NUMBER] [char](3) NULL,
	[PD_OIL_PREV_ITEM_NUMBER] [int] NULL,
	[PD_OIL_PREV_CHANGED_FLAG] [char](1) NULL,
	[PD_OIL_PREV_FILED_BY_EDI_FLAG] [char](1) NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[PDOPRVAL]    Script Date: 2/19/2025 2:46:37 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PDOPRVAL](
	[RRC_TAPE_RECORD_ID] [char](2) NOT NULL,
	[PD_OIL_ALLOW_DAILY_BBLS_HIST] [decimal](9, 0) NULL,
	[PD_GAS_LIMIT_DAILY_MCF_HIST] [decimal](9, 0) NULL,
	[PD_OIL_ALLOW_EFF_YEAR_HIST] [smallint] NULL,
	[PD_OIL_ALLOW_EFF_MONTH_HIST] [tinyint] NULL,
	[PD_OIL_ALLOW_EFF_DAY_HIST] [tinyint] NULL,
	[PD_OIL_ALLOW_ISS_YEAR_HIST] [smallint] NULL,
	[PD_OIL_ALLOW_ISS_MONTH_HIST] [tinyint] NULL,
	[PD_OIL_ALLOW_ISS_DAY_HIST] [tinyint] NULL,
	[PD_OIL_ALLOW_CYCLE_BBLS_HIST] [decimal](9, 0) NULL,
	[PD_GAS_LIMIT_CYCLE_MCF_HIST] [decimal](9, 0) NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[PDORMVDS]    Script Date: 2/19/2025 2:46:37 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PDORMVDS](
	[RRC_TAPE_RECORD_ID] [char](2) NOT NULL,
	[PD_O_REMOVED_CODE_6_DISCREP] [char](1) NOT NULL,
	[PD_O_REMOVED_R_3_DISCREP] [char](1) NOT NULL,
	[PD_O_REMOVED_NET_OIL_DISCREP] [char](1) NOT NULL,
	[PD_O_REMOVED_LSE_SEV_DISCREP] [char](1) NOT NULL,
	[PD_O_REMOVED_OTHER_DISCREP] [char](1) NOT NULL,
	[PD_O_REMOVED_LOST_OIL_DISCREP] [char](1) NOT NULL,
	[PD_O_REMOVED_CIRC_OIL_DISCREP] [char](1) NOT NULL,
	[PD_O_REMOVED_PRD_INACT_DISCREP] [char](1) NOT NULL,
	[PD_O_REMOVED_FILE_COMM_DISCREP] [char](1) NOT NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[PDORPTCY]    Script Date: 2/19/2025 2:46:37 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PDORPTCY](
	[RRC_TAPE_RECORD_ID] [char](2) NOT NULL,
	[PD_OIL_RPT_CYCLE_KEY] [int] NOT NULL,
	[PD_DAILY_OIL_PRORATED_ALLOW] [decimal](9, 0) NULL,
	[PD_DAILY_OIL_EXEMPT_ALLOW] [decimal](9, 0) NULL,
	[PD_DAILY_CSH_PRORATED_ALLOW] [decimal](9, 0) NULL,
	[PD_DAILY_CSH_EXEMPT_ALLOW] [decimal](9, 0) NULL,
	[PD_OIL_ALLOWABLE_CYCLE_BBLS] [decimal](9, 0) NULL,
	[PD_CSH_LIMIT_CYCLE_MCF] [decimal](9, 0) NULL,
	[PD_OIL_ALLOW_EFF_YEAR] [smallint] NULL,
	[PD_OIL_ALLOW_EFF_MONTH] [tinyint] NULL,
	[PD_OIL_ALLOW_EFF_DAY] [tinyint] NULL,
	[PD_OIL_ALLOW_ISS_YEAR] [smallint] NULL,
	[PD_OIL_ALLOW_ISS_MONTH] [tinyint] NULL,
	[PD_OIL_ALLOW_ISS_DAY] [tinyint] NULL,
	[PD_OIL_ENDING_BALANCE] [decimal](9, 0) NULL,
	[PD_PRESENT_OIL_STATUS] [decimal](9, 0) NULL,
	[PD_PRESENT_CSGHD_STATUS] [decimal](9, 0) NULL,
	[PD_ADJUSTED_OIL_STATUS] [decimal](9, 0) NULL,
	[PD_ADJUSTED_CSGHD_STATUS] [decimal](9, 0) NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[PDREMARK]    Script Date: 2/19/2025 2:46:37 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PDREMARK](
	[RRC_TAPE_RECORD_ID] [char](2) NOT NULL,
	[PD_REMARK_NUMBER] [int] NOT NULL,
	[PD_REMARK_LINE_NO] [int] NOT NULL,
	[PD_REMARK_TEXT] [varchar](70) NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[PDROOT_OIL]    Script Date: 2/19/2025 2:46:37 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PDROOT_OIL](
	[RRC_TAPE_RECORD_ID] [char](2) NOT NULL,
	[PD_OIL_CODE] [char](1) NOT NULL,
	[PD_OIL_DISTRICT] [smallint] NOT NULL,
	[PD_OIL_LEASE_NBR] [int] NOT NULL,
	[PD_MOVABLE_BALANCE] [decimal](9, 0) NULL,
	[PD_BEGINNING_OIL_STATUS] [decimal](9, 0) NULL,
	[PD_BEGINNING_CSGHD_STATUS] [decimal](9, 0) NULL,
	[PD_OIL_OLDEST_EOM_BALANCE] [decimal](9, 0) NULL
) ON [PRIMARY]
GO
USE [master]
GO
ALTER DATABASE [EBCDIC] SET  READ_WRITE 
GO
