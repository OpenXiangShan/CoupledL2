package oceanus.chi

import chisel3._

package object field {

    /*
    * Convert CHI Field enumerations to literal UInt instance. 
    */
    implicit class fromEnumCHIFieldSizeToUInt(field: EnumCHIFieldSize) {
        def U: UInt = field.value.U
    }

    implicit class fromEnumCHIFieldOrderToUInt(field: EnumCHIFieldOrder) {
        def U: UInt = field.value.U
    }

    implicit class fromEnumCHIFieldRespErrToUInt(field: EnumCHIFieldRespErr) {
        def U: UInt = field.value.U
    }
    /**/
}

