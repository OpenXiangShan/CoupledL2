#!/bin/bash
db_path=$1
shift

while getopts ":x:w:l:" OPT; do
  case $OPT in
    x)
      CHI_FIELD=", $OPTARG"
      ;;
    w)
      CONDITION="where $OPTARG"
      ;;
    l)
      LIMIT="limit $OPTARG"
      ;;
    \?)
      echo "Unknown Option: -$OPTARG"
      echo "Usage: $0 db_path [-x other_chi_fields] [-w condition] [-l limit]"
      echo "Example:"
      echo "       $0 /path/to/chisel.db -x TXNID -w 'ADDR=0x0' -l 10"
      exit 1
      ;;
  esac
done


sqlite3 $db_path "select STAMP, SITE, ADDR, CHANNEL, OPCODE, DATA_0, DATA_1, DATA_2, DATA_3 $CHI_FIELD from CHILog $CONDITION $LIMIT" | sed 's/|/ /g' | awk --bignum '

func chnstr(chn) {
  split("TXREQ RXRSP RXDAT RXSNP TXRSP TXDAT", channels, " ");
  return channels[chn + 1]
}

func opstr(chn, op) {
  req[1] = "ReqLCrdReturn"
  req[2] = "ReadShared"
  req[3] = "ReadClean"
  req[4] = "ReadOnce"
  req[5] = "ReadNoSnp"
  req[6] = "PCrdReturn"
  req[8] = "ReadUnique"
  req[9] = "CleanShared"
  req[10] = "CleanInvalid"
  req[11] = "MakeInvalid"
  req[12] = "CleanUnique"
  req[13] = "MakeUnique"
  req[14] = "Evict"
  req[21] = "DVMOp"
  req[22] = "WriteEvictFull"
  req[24] = "WriteCleanFull"
  req[25] = "WriteUniquePtl"
  req[26] = "WriteUniqueFull"
  req[27] = "WriteBackPtl"
  req[28] = "WriteBackFull"
  req[29] = "WriteNoSnpPtl"
  req[30] = "WriteNoSnpFull"
  req[33] = "WriteUniqueFullStash"
  req[34] = "WriteUniquePtlStash"
  req[35] = "StashOnceShared"
  req[36] = "StashOnceUnique"
  req[37] = "ReadOnceCleanInvalid"
  req[38] = "ReadOnceMakeInvalid"
  req[39] = "ReadNotSharedDirty"
  req[40] = "CleanSharedPersist"
  req[41] = "AtomicStore_ADD"
  req[42] = "AtomicStore_CLR"
  req[43] = "AtomicStore_EOR"
  req[44] = "AtomicStore_SET"
  req[45] = "AtomicStore_SMAX"
  req[46] = "AtomicStore_SMIN"
  req[47] = "AtomicStore_UMAX"
  req[48] = "AtomicStore_UMIN"
  req[49] = "AtomicLoad_ADD"
  req[50] = "AtomicLoad_CLR"
  req[51] = "AtomicLoad_EOR"
  req[52] = "AtomicLoad_SET"
  req[53] = "AtomicLoad_SMAX"
  req[54] = "AtomicLoad_SMIN"
  req[55] = "AtomicLoad_UMAX"
  req[56] = "AtomicLoad_UMIN"
  req[57] = "AtomicSwap"
  req[58] = "AtomicCompare"
  req[59] = "PrefetchTgt"

  resp[1] = "RespLCrdReturn"
  resp[2] = "SnpResp"
  resp[3] = "CompAck"
  resp[4] = "RetryAck"
  resp[5] = "Comp"
  resp[6] = "CompDBIDResp"
  resp[7] = "DBIDResp"
  resp[8] = "PCrdGrant"
  resp[9] = "ReadReceipt"
  resp[10] = "SnpRespFwded"

  snp[1] = "SnpLCrdReturn"
  snp[2] = "SnpShared"
  snp[3] = "SnpClean"
  snp[4] = "SnpOnce"
  snp[5] = "SnpNotSharedDirty"
  snp[6] = "SnpUniqueStash"
  snp[7] = "SnpMakeInvalidStash"
  snp[8] = "SnpUnique"
  snp[9] = "SnpCleanShared"
  snp[10] = "SnpCleanInvalid"
  snp[11] = "SnpMakeInvalid"
  snp[12] = "SnpStashUnique"
  snp[13] = "SnpStashShared"
  snp[14] = "SnpDVMOp"
  snp[18] = "SnpSharedFwd"
  snp[19] = "SnpCleanFwd"
  snp[20] = "SnpOnceFwd"
  snp[21] = "SnpNotSharedDirtyFwd"
  snp[24] = "SnpUniqueFwd"

  data[1] = "DataLCrdReturn"
  data[2] = "SnpRespData"
  data[3] = "CopyBackWrData"
  data[4] = "NonCopyBackWrData"
  data[5] = "CompData"
  data[6] = "SnpRespDataPtl"
  data[7] = "SnpRespDataFwded"
  data[8] = "WriteDataCancel"

  ret = "Unknown OP"
  switch(chn) {
    case 0: # txreq
      ret = req[op+1]
      break;
    case 1: # rxrsp
      ret = resp[op+1]
      break;
    case 2: # rxdat
      ret = data[op+1]
      break;
    case 3: # rxsnp
      ret = snp[op+1]
      break;
    case 4: # txrsp
      ret = resp[op+1]
      break;
    case 5: # txdat
      ret = data[op+1]
      break;
  }
  return ret
}

{
  addr = $3
  chn = $4
  op = $5
  data_1 = $6
  data_2 = $7
  data_3 = $8
  data_4 = $9

  $3 = sprintf("%lx(%ld)", addr, addr)
  $4 = chnstr(chn)
  $5 = sprintf("%-20s", opstr(chn, op))

  if (chn == 2 || chn == 5) {
    $6 = sprintf("%016lx", data_1)
    $7 = sprintf("%016lx", data_2)
    $8 = sprintf("%016lx", data_3)
    $9 = sprintf("%016lx", data_4)
  } else {
    $6 = sprintf("%016s", "")
    $7 = sprintf("%016s", "")
    $8 = sprintf("%016s", "")
    $9 = sprintf("%016s", "")
  }
}

1                                   # print every line
'



# *** all CHI Fields ***
# ID
# RSVDC
# TRACETAG
# EXPCOMPACK
# SNOOPME
# LPID
# SNPATTR
# MEMATTR_EWA
# MEMATTR_DEVICE
# MEMATTR_CACHEABLE
# MEMATTR_ALLOCATE
# PCRDTYPE
# ALLOWRETRY
# LIKELYSHARED
# NS
# ADDR
# SIZE
# RETURNTXNID
# STASHNIDVALID
# RETURNNID
# TXNID
# SRCID
# TGTID
# QOS
# DBID
# FWDSTATE
# RESP
# RESPERR
# DATA_0
# DATA_1
# DATA_2
# DATA_3
# BE
# DATAID
# CCID
# HOMENID
# RETTOSRC
# DONOTGOTOSD
# FWDTXNID
# FWDNID
# CHANNEL
# ORDERING
# OPCODE
# STAMP
# SITE
