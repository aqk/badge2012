'' =================================================================================================
''
''   File....... i2c_basics.spin
''   Purpose.... Low- and mid-level I2C routines
''   Author..... Jon "JonnyMac" McPhalen
''               Terms of Use: MIT License
''               -- elements inspired by code from Mike Green
''               -- see below for terms of use
''   E-mail..... jon@jonmcphalen.com
''   Started.... 28 JUL 2009
''   Updated.... 24 JAN 2012
''
'' =================================================================================================

{{

   Low/Mid-level I2C routines.
 
   Code does not drive SCL or SDA pins high; pull-ups are required on both pins.

   Use "x" routines for a device with that uses a 16-bit address (e.g., 32k/64k EEPROM)
  
}}   


con

   #0, ACK, NAK

  #28, BOOT_SCL, BOOT_SDA                                       ' Propeller I2C pins


var

  long  scl
  long  sda
    

pub setup

'' Setup I2C using default (boot EEPROM) pins

  setupx(BOOT_SCL, BOOT_SDA)
         

pub setupx(sclpin, sdapin)

'' Define I2C SCL (clock) and SDA (data) pins

  longmove(@scl, @sclpin, 2)                                    '  copy pins
  dira[scl] := 0                                                '  float to pull-up
  outa[scl] := 0                                                '  write 0 to output reg
  dira[sda] := 0
  outa[sda] := 0

  repeat 9                                                      ' reset device
    dira[scl] := 1
    dira[scl] := 0
    if (ina[sda])
      quit


pub wr_byte(id, addr, value) | ackbit

'' Write byte to device w/8-bit addressing
'' --   id : device slave id
'' -- addr : 8-bit address in the device

  ackbit := wr_page(id, addr, 1, @value)

  return ackbit


pub wr_word(id, addr, value) | ackbit

'' Write word to device w/8-bit addressing
'' --   id : device slave id
'' -- addr : 8-bit address in the device  

  ackbit := wr_page(id, addr, 2, @value)

  return ackbit


pub wr_long(id, addr, value) | ackbit

'' Write long to device w/8-bit addressing
'' --   id : device slave id
'' -- addr : 8-bit address in the device

  ackbit := wr_page(id, addr, 4, @value)

  return ackbit
   

pub wr_page(id, addr, n, src) | ackbit

'' Write n bytes from src to device w/8-bit addressing
'' --   id : device slave id
'' -- addr : 8-bit address in the device
'' --    n : bytes to write
'' --  src : pointer to source value(s)
''
'' Be mindful of address/page size in device to prevent page wrap-around

  wait(id)
  write(addr)                                                   ' lsb of address
  ackbit := ACK                                                 ' assume okay
  repeat n
    ackbit |= write(byte[src++])                                ' write a byte 
  stop

  return ackbit


pub wr_str(id, addr, src) | ackbit, b

'' Write (arbitrary-length) z-string at src to device w/8-bit addressing
'' --   id : device slave id
'' -- addr : 8-bit address in the device
'' --  src : pointer to z-string    

  if (addr => 0)
    ackbit := ACK                                               ' assume okay  
    repeat
      b := byte[src++]                                          ' get byte from string
      ackbit |= wr_byte(id, addr++, b)                          ' write to card
      if (b == 0)                                               ' end of string?
        quit                                                    '  yes, we're done

  else
    ackbit := NAK
    
  return ackbit
     

pub rd_byte(id, addr) | value

'' Read byte from device w/8-bit addressing
'' --   id : device slave id
'' -- addr : 8-bit address in the device

  rd_page(id, addr, 1, @value)
  
  return value & $0000_00FF


pub rd_word(id, addr) | value

'' Read word from device w/8-bit addressing
'' --   id : device slave id
'' -- addr : 8-bit address in the device

  rd_page(id, addr, 2, @value)

  return value & $0000_FFFF


pub rd_long(id, addr) | value

'' Read long from device w/8-bit addressing
'' --   id : device slave id
'' -- addr : 8-bit address in the device

  rd_page(id, addr, 4, @value)

  return value 


pub rd_page(id, addr, n, dest)

'' Read n bytes from device w/8-bit addressing; output to dest
'' --   id : device slave id (e.g., $A0 for EEPROM)
'' -- addr : 8-bit address in the device
'' --    n : bytes to read
'' -- dest : pointer to the destination
''
'' Be mindful of address/page size in device to prevent page wrap-around

  wait(id)
  write(addr)                                                   ' lsb of address
  start                                                         ' restart for read
  write(id | $01)                                               ' device read
  repeat (n - 1)
    byte[dest++] := read(ACK)
  byte[dest] := read(NAK)                                       ' last byte gets NAK
  stop 

  
pub rd_str(id, addr, dest) | b

'' Read (arbitrary-length) z-string, store at dest
'' --   id : device slave id
'' -- addr : 8-bit address in the device
'' -- dest : pointer to the destination 

  if (addr => 0)
    repeat
      b := rd_byte(id, addr++)                                  ' read byte from card
      byte[dest++] := b                                         ' write to string  
      if (b == 0)                                               ' at end?
        quit                                                    '  if yes, we're done
  

con

pub wr_bytex(id, addr, value) | ackbit

'' Write byte to device w/16-bit addressing
'' --   id : device slave id
'' -- addr : 16-bit address in the device

  ackbit := wr_pagex(id, addr, 1, @value)

  return ackbit


pub wr_wordx(id, addr, value) | ackbit

'' Write word to device w/16-bit addressing
'' --   id : device slave id
'' -- addr : 16-bit address in the device

  ackbit := wr_pagex(id, addr, 2, @value)

  return ackbit


pub wr_longx(id, addr, value) | ackbit

'' Write long to device w/16-bit addressing
'' --   id : device slave id
'' -- addr : 16-bit address in the device

  ackbit := wr_pagex(id, addr, 4, @value)

  return ackbit  


pub wr_pagex(id, addr, n, src) | ackbit

'' Write n bytes from src to device w/16-bit addressing
'' --   id : device slave id
'' -- addr : 16-bit address in the device
'' --    n : bytes to write
'' --  src : pointer to source value
''
'' Be mindful of address/page size in device to prevent page wrap-around

  wait(id)
  write(addr.byte[1])                                           ' msb of address
  write(addr.byte[0])                                           ' lsb of address
  ackbit := ACK                                                 ' assume okay
  repeat n
    ackbit |= write(byte[src++])                                ' write a byte 
  stop

  return ackbit


pub wr_strx(id, addr, src) | ackbit, b

'' Write string at src to device w/16-bit addressing
'' --   id : device slave id
'' -- addr : 16-bit address in the device
'' --  src : pointer to z-string    

  if (addr => 0)
    ackbit := ACK                                               ' assume okay  
    repeat
      b := byte[src++]                                          ' get byte from string
      ackbit |= wr_bytex(id, addr++, b)                         ' write to card
      if (b == 0)                                               ' end of string?
        quit                                                    '  yes, we're done

  else
    ackbit := NAK
    
  return ackbit  


pub rd_bytex(id, addr) | value

'' Return byte value from device w/16-bit addressing
'' --   id : device slave id
'' -- addr : 16-bit address in the device

  rd_pagex(id, addr, 1, @value)

  return value & $0000_00FF


pub rd_wordx(id, addr) | value

'' Return word value from device w/16-bit addressing
'' --   id : device slave id
'' -- addr : 16-bit address in the device

  rd_pagex(id, addr, 2, @value)

  return value & $0000_FFFF
  

pub rd_longx(id, addr) | value

'' Return long value from device w/16-bit addressing
'' --   id : device slave id
'' -- addr : 16-bit address in the device

  rd_pagex(id, addr, 4, @value)

  return value


pub rd_pagex(id, addr, n, dest)

'' Read n bytes from device w/8-bit addressing; output to dest
'' --   id : device slave id (e.g., $A0 for EEPROM)
'' -- addr : 16-bit address in the device
'' --    n : bytes to read
'' -- dest : pointer to the destination
''
'' Be mindful of address/page size in device to prevent page wrap-around

  wait(id)
  write(addr.byte[1])                                           ' msb of address
  write(addr.byte[0])                                           ' lsb of address
  start                                                         ' restart for read
  write(id | $01)                                               ' device read
  repeat while (n > 1)
    byte[dest++] := read(ACK)
    --n
  byte[dest] := read(NAK)                                       ' last byte gets NAK
  stop 


pub rd_strx(id, addr, dest) | b

'' Read (arbitrary-length) z-string, store at dest
'' --   id : device slave id
'' -- addr : 16-bit address in the device
'' -- dest : pointer to the destination 

  if (addr => 0)
    repeat
      b := rd_bytex(id, addr++)                                 ' read byte from device
      byte[dest++] := b                                         ' write to string  
      if (b == 0)                                               ' at end?
        quit                                                    '  if yes, we're done 


con
        
pub wait(id) | ackbit

'' Waits for I2C device to be ready for new command

  repeat
    start
    ackbit := write(id & $FE)
  until ackbit == ACK


pub start

'' Create I2C start sequence
'' -- will wait if I2C buss SDA pin is held low

  dira[sda] := 0                                                ' float SDA (1)
  dira[scl] := 0                                                ' float SCL (1)
  repeat while (ina[scl] == 0)                                  ' allow "clock stretching"

  outa[sda] := 0
  dira[sda] := 1                                                ' SDA low (0)

  
pub write(i2cbyte) | ackbit

'' Write byte to I2C buss

  outa[scl] := 0
  dira[scl] := 1                                                ' SCL low

  i2cbyte <<= constant(32-8)                                    ' move msb (bit7) to bit31
  repeat 8                                                      ' output eight bits
    dira[sda] := ((i2cbyte <-= 1) ^ 1)                          ' send msb first
    dira[scl] := 0                                              ' SCL high (float to p/u)
    dira[scl] := 1                                              ' SCL low

  dira[sda] := 0                                                ' relase SDA to read ack bit
  dira[scl] := 0                                                ' SCL high (float to p/u)  
  ackbit := ina[sda]                                            ' read ack bit
  dira[scl] := 1                                                ' SCL low

  return (ackbit & 1)


pub read(ackbit) | i2cbyte

'' Read byte from I2C buss

  outa[scl] := 0                                                ' prep to write low
  dira[sda] := 0                                                ' make input for read

  repeat 8
    dira[scl] := 0                                              ' SCL high (float to p/u)
    i2cbyte := (i2cbyte << 1) | ina[sda]                        ' read the bit
    dira[scl] := 1                                              ' SCL low
                             
  dira[sda] := !ackbit                                          ' output ack bit 
  dira[scl] := 0                                                ' clock it
  dira[scl] := 1

  return (i2cbyte & $FF)


pub stop

'' Create I2C stop sequence 

  outa[sda] := 0
  dira[sda] := 1                                                ' SDA low
  
  dira[scl] := 0                                                ' float SCL
  repeat while (ina[scl] == 0)                                  ' hold for clock stretch
  
  dira[sda] := 0                                                ' float SDA


dat

{{

  Terms of Use: MIT License

  Permission is hereby granted, free of charge, to any person obtaining a copy of this
  software and associated documentation files (the "Software"), to deal in the Software
  without restriction, including without limitation the rights to use, copy, modify,
  merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
  permit persons to whom the Software is furnished to do so, subject to the following
  conditions:

  The above copyright notice and this permission notice shall be included in all copies
  or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
  INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
  PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
  CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
  OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

}}