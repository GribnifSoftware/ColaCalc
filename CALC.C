#include "mwclinea.h"
#include "tos.h"
#include "string.h"

#define sshiftmod  (*(char *)0x44CL)
#define logbase    (*(char **)0x44e)
#define _VDO_COOKIE 0x5F56444FL
#define VERSION    "ColaCalc 1.4 by Dan Wilga"
#define INIT       "\r\n\
    ColaCalc 1.4 by Dan Wilga\r\n\
Copyright ½ 1997, Gribnif Sofware\r\n"

extern void gtext( int x, char *s );
extern int linea0(void);

int bigscr, fht, gt1, gt2, gt3, mx;
unsigned char *fdat;
long scan_off, scr_off, disp_siz, cdisp_siz, user_disp;
char *disp, active_sh=4, active_ch=0x61, errstr[]="-E-", is_CD, falc_vid;

#define put_str(s,x) gtext(x,s)

void bot_line( int flag )
{
  void longcpy( void *to, void *from, long size );
  void longcpyc( void *to, void *from, long size );
  
  if( !flag ) longcpyc( disp, logbase+scr_off, cdisp_siz>>2 );
  else longcpy( logbase+scr_off, disp, cdisp_siz>>2 );
}

int base=16, width=8;
char basetab[]={ 2, 8, 10, 16 }, basen=3, wordsize=0, sign,
    hexstr[]="0123456789ABCDEF", widtab[4][3]={ 32, 16, 8, 10, 6, 3,
    10, 5, 3, 8, 4, 2 }, *old_fname="   ";
unsigned long accum, last, fkey[10];

void to_base( unsigned long n, char *b, int s, int base, int wid )
{
  int i, w;
  char nb[35];

  if( wordsize==1 )
    if( !sign ) n = (unsigned int)n;
    else (long)n = (int)n;
  else if( wordsize==2 )
    if( !sign ) n = (unsigned char)n;
    else (long)n = (char)n;
  i = 0;
  if( !n ) nb[i++] = '0';
  else
  {
    if( s && (n&0x80000000L) ) n = (unsigned long)(-(long)n);
    else s = 0;
    for (; n; n /= base)
    {
      nb[i++] = hexstr[n%base];
      if( base==2 && n )
        if( i==26 || i==8 ) nb[i++] = 'ø';
        else if( i==17 ) nb[i++] = 'ù';
    }
    if( s ) nb[i++] = '-';
  }
  w = wid - i;
  while (w-- > 0) nb[i++] = ' ';
  while (i--)
    *b++ = nb[i];
  *b = '\0';
}

void display(void)
{
  char buf[36];
  
  to_base( accum, buf, sign, base, 35 );
  put_str( buf, 32 );
}

char *wsiz="lwb", *bse="bodh";

void wordsiz(void)
{
  char buf[4];
  
  buf[0] = sign ? 's' : 'u';
  buf[1] = wsiz[wordsize];
  buf[2] = bse[basen];
  buf[3] = '\0';
  put_str( buf, 0 );
}

int f_plus(int i)
{
  if(!i) return(1);
  accum = accum + last;
  return(1);
}

int f_minus(int i)
{
  if(!i) return(1);
  accum = last - accum;
  return(1);
}

int f_times(int i)
{
  if(!i) return(1);
  if( !sign ) accum = last * accum;
  else (long)accum = (long)last * (long)accum;
  return(1);
}

int f_div(int i)
{
  if(!i) return(1);
  if( !accum ) return(0);
  if( !sign ) accum = last / accum;
  else (long)accum = (long)last / (long)accum;
  return(1);
}

int newsize( int new )
{
  if( new == wordsize ) return(0);
  if( wordsize==1 )
    if( !sign ) accum = (unsigned int)accum;
    else (long)accum = (int)accum;
  else if( wordsize==2 )
    if( !sign ) accum = (unsigned char)accum;
    else (long)accum = (char)accum;
  wordsize = new;
  return(-1);  
}

int f_byte()
{
  return( newsize(2) );
}

int f_word()
{
  return( newsize(1) );
}

int f_long()
{
  return( newsize(0) );
}

int f_unsign()
{
  if( sign )
  {
    sign = 0;
    return(-2);
  }
  return(0);
}

int f_sign()
{
  if( !sign )
  {
    sign = 1;
    return(-2);
  }
  return(0);
}

int newbase( int i )
{
  if( i != basen )
  {
    basen = i;
    return(-1);
  }
  return(0);
}

int f_binary()
{
  return(newbase(0));
}

int f_octal()
{
  return(newbase(1));
}

int f_dec()
{
  return(newbase(2));
}

int f_hex()
{
  return(newbase(3));
}

int f_mod( int i )
{
  if(!i) return(1);
  if( !sign ) accum = last % accum;
  else (long)accum = (long)last % (long)accum;
  return(1);
}

int f_not()
{
  accum = accum ^ -1L;
  return(3);
}

int f_neg()
{
  accum = -accum;
  return(3);
}

int f_and( int i )
{
  if(!i) return(1);
  accum = last & accum;
  return(1);
}

int f_or( int i )
{
  if(!i) return(1);
  accum = last | accum;
  return(1);
}

int f_xor( int i )
{
  if(!i) return(1);
  accum = last ^ accum;
  return(1);
}

int f_ror( int i )
{
  if(!i) return(1);
  accum &= 0x1F;
  while( accum-- )
    last = (last>>1) | (last<<(widtab[0][wordsize]-1));
  accum = last;
  return(1);
}

int f_rol( int i )
{
  if(!i) return(1);
  accum &= 0x1F;
  while( accum-- )
    last = (last<<1) | ((last>>(widtab[0][wordsize]-1))&1);
  accum = last;
  return(1);
}

int f_asr( int i )
{
  if(!i) return(1);
  if( sign ) (long)accum = (long)last >> (char)accum;
  else accum = last >> (char)accum;
  return(1);
}

int f_asl( int i )
{
  if(!i) return(1);
  accum = last<<accum;
  return(1);
}

int f_lpeek()
{
  if( accum&1 ) return(2);
  accum = *(unsigned long *)accum;
  return(3);
}

int f_wpeek()
{
  if( accum&1 ) return(2);
  if( !sign ) accum = *(unsigned int *)accum;
  else (long)accum = *(int *)accum;
  return(3);
}

int f_bpeek()
{
  if( !sign ) accum = *(unsigned char *)accum;
  else (long)accum = *(char *)accum;
  return(3);
}

#define FUNCS 26

struct
{
  char sh, ch, *name;
  int (*fn)( int check );
} func[FUNCS] = { { -1, '+', "+  ", f_plus },
                  { -1, '-', "-  ", f_minus },
                  { -1, '*', "*  ", f_times },
                  { -1, '/', "/  ", f_div },
                  { -1, '%', "MOD", f_mod },
                  { -1, '!', 0L, f_not },
                  { -1, '\x7f', 0L, f_neg },
                  { -1, '&', "AND", f_and },
                  { -1, '|', "OR ", f_or },
                  { -1, '^', "XOR", f_xor },
                  { 0, '\x4B', "ROL", f_rol },
                  { 4, '\x73', "ASL", f_asl },
                  { 0, '\x4D', "ROR", f_ror },
                  { 4, '\x74', "ASR", f_asr },
                  { 8, '\x19', 0L, f_bpeek },
                  { 2, '\x19', 0L, f_wpeek },
                  { 4, '\x19', 0L, f_lpeek },
                  { 8, '\x30', 0L, f_byte },
                  { 8, '\x11', 0L, f_word },
                  { 8, '\x26', 0L, f_long },
                  { 0, '\x16', 0L, f_unsign },
                  { 0, '\x1f', 0L, f_sign },
                  { 4, '\x30', 0L, f_binary },
                  { 4, '\x18', 0L, f_octal },
                  { 4, '\x20', 0L, f_dec },
                  { 4, '\x23', 0L, f_hex },
                  };

void skip_space( char **buf, int *size )
{
  while( *size && (**buf==' ' || **buf=='\t') )
  {
    (*buf)++;
    (*size)--;
  }
}

void skip_eol( char **buf, int *size )
{
  while( *size && (**buf!='\r' && **buf!='\n') )
  {
    (*buf)++;
    (*size)--;
  }
  while( *size && (**buf=='\r' || **buf=='\n') )
  {
    (*buf)++;
    (*size)--;
  }
}

unsigned long from_hex( char **buf, int *size )
{
  char *ptr;
  unsigned long r=0;
  
  while( *size )
  {
    if( **buf == ' ' || **buf == '\t' ) return(r);
    if( **buf >= 'a' && **buf <= 'f' ) **buf &= 0x5f;
    if( (ptr=strchr(hexstr,**buf)) != 0 ) r = (r<<4)+(ptr-hexstr);
    else r = **buf;
    (*buf)++;
    (*size)--;
  }
  return(r);
}

void read_key( char *sh, char *ch, char **buf, int *size )
{
  skip_space( buf, size );
  if( size )
    if( (*sh=from_hex( buf, size ))=='*' ) *sh = -1;
  skip_space( buf, size );
  if( size ) *ch = from_hex( buf, size );
  skip_eol( buf, size );
}

void load_cfg( char *buf, long s )
{
  int h, i, size;
  extern char in_auto;
  static char path[]="\\AUTO\\COLACALC.DAT";
  char *ptr;
  
  if( s<=0 ) return;
  if( (h=Fopen( in_auto ? path : path+6, 0 )) > 0 )
  {
    if( (size=Fread( h, s, buf )) > 0 )
    {
      sign = *buf++ == 's';
      if( (ptr=strchr(wsiz,*buf++)) != 0 ) wordsize = ptr-wsiz;
      if( (ptr=strchr(bse,*buf++)) != 0 ) basen = ptr-bse;
      base = basetab[basen];
      width = widtab[basen][wordsize] + sign;
      size -= 3;
      skip_eol( &buf, &size );
      read_key( &active_sh, &active_ch, &buf, &size );
      for( i=0; size && i<FUNCS; i++ )
        read_key( &func[i].sh, &func[i].ch, &buf, &size );
      for( i=0; size && i<10; i++ )
      {
        skip_space( &buf, &size );
        if( size ) fkey[i] = from_hex( &buf, &size );
        skip_eol( &buf, &size );
      }
      if( size )
      {
        skip_space( &buf, &size );
        if( size ) user_disp = from_hex( &buf, &size );
      }
    }
    Fclose(h);
  }
}

void fname( char *s )
{
  if( s != errstr ) old_fname=s;
  if( mx>=35+5+2 ) put_str( s, (35+5)<<3 );
}

void fkeys(void)
{
  int i;
  char buf[11];
  
  if( mx>=79 )
  {
    for( i=0; i<10; i++ )
      buf[i] = fkey[i] ? (i<9?i+'1':'0') : '.';
    buf[10] = '\0';
    put_str( buf, (mx<<3) - (sizeof(VERSION)-1<<3) - 10*8 );
  }
}

void init(void)
{
  wordsiz();
  display();
  fkeys();
}

#ifdef DEBUG
  #define iskey Bconstat(2)
  #define getkey Bconin(2)
  #define kbshift Kbshift(-1)
  void to_kbbuf(void){}
#else
  extern IOREC *io;
  #define keyhead io->ibufhd
  #define keytail io->ibuftl
  extern char *kbshift;
  #define iskey keyhead != keytail
  #define getkey keypress()
  #define kbshift *kbshift
  #define conterm (*(char *)0x484)
  extern KEYTAB *keytab;
  long keypress(void)
  {
    while( keyhead==keytail );
    if( (keyhead+=4) >= io->ibufsiz ) keyhead=0;
    return(*(long *)((long)io->ibuf+keyhead));
  }
  int keytbl( char *tbl, unsigned char c )
  {
    int i;
    
    for( i=0; i<128; i++ )
      if( *tbl++ == c ) return(i);
    return(-1);
  }
  #define vblq *(long **)0x456
  #define nvbls *(int *)0x454
  long *vblptr;
  unsigned char vblbuf[36], *vblbptr;

  void vbl(void)
  {    
    int i;
    unsigned int j;
    long l;
  
    if( (l=*vblbptr) == 0 )
    {
      *vblptr = 0L;
      vblptr = 0L;
    }
    else if( keyhead==keytail )
    {
      vblbptr++;
      if( (i=keytbl( keytab->unshift, (unsigned char)l )) >= 0 ) l |= (long)i<<16;
      else if( (i=keytbl( keytab->shift, (unsigned char)l )) >= 0 )
      {
        if( conterm&8 ) l |= 1L<<24;
        l |= (long)i<<16;
      }
      if( (j = keytail+4) >= io->ibufsiz ) j = 0;
      *(long *)((long)io->ibuf+j) = l;
      keytail = j;
    }
  }
  void to_kbbuf(void)
  {
    long *ptr;
    int i;

    for( i=nvbls, ptr=vblq; i>=0; i--, ptr++ )
      if( !*ptr )
      {
        to_base( accum, (char *)(vblbptr=vblbuf), sign, base, 0 );
        *(vblptr = ptr) = (long)vbl;
        return;
      }
  }
#endif

int do_func( int fnc( int i ) )
{
  if( (*fnc)(1) )
  {
    display();
    return(0);
  }
  fname( errstr );
  return(1);
}

int is_highprec( int fnc( int i ) )
{
  return( fnc==f_times || fnc==f_div );
}

void breakpoint(void) 0x4afc;

void calc(void)
{
  int done=0, digits=0, i;
  long l;
  unsigned long last2=0L;
  char sh, s, *ptr, was_eq=1, err=0, keymode=0, buf[4];
  static char keystr[]="Shift = $   Scan = $   Char = $    ";
  int (*fnc)( int check ) = 0L;
  int (*fnc2)( int check ) = 0L;
  int p;

#ifndef DEBUG
  if( vblptr )
  {
    *vblptr = 0L;
    vblptr = 0L;
  }
#endif
  linea0();
  fht = V_CEL_MY >= 399 ? 16 : V_CEL_HT;
  i = V_CEL_MY;
  p = VPLANES;
  bigscr = (l=(long)logbase) >= 0xC00000L && l < 0xE00000L ? p : -1;
  if( l==0xfec00000L )
  {
    bigscr = p<8 ? 1 : p;
    is_CD = 1;
    scan_off = V_X_MAX>>3;
    if( p>=8 ) scan_off *= p;
  }
  else scan_off = VWRAP;
  if( (cdisp_siz=(long)fht*scan_off) > disp_siz )
    if( fht==16 )	/* use 8x8 if area is larger than buffer */
    {
      if( (cdisp_siz=(long)(fht=8)*scan_off) > disp_siz ) return;
      i = (i<<1) + 1;
    }
    else return;
  scr_off = (long)i * fht * scan_off;
  fdat = la_init.li_a1[1+(fht==16)]->font_data;
  mx = V_CEL_MX;
  if( is_CD ) p = bigscr;
  if( p<=16 )
    for( i=0; ;i++ )
      if( (1<<i) == p )
      {
        gt3=i+1;
        break;
      }
  if( (i=bigscr)<0 )
    if( (i=sshiftmod) < 2 ) i = !i ? 4 : 2;
    else i = p;
  gt1 = i;
  gt2 = (i-1)<<1;
  bot_line(0);
  init();
  if( mx>=79 ) put_str( VERSION, (mx<<3)-(sizeof(VERSION)-2<<3) );
  while( !done )
  {
    l = getkey;
    sh = (kbshift)&0xf;
    i = (char)(l>>16);
    if( !keymode && (char)l >='a' && (char)l <= 'f' ) (char)l &= 0x5f;
    if( !sh && i==0x62 )
      if( (keymode ^= 1) != 0 ) put_str( "KEY", 0 );
      else
      {
        init();
        fname( old_fname );
      }
    if( keymode )
    {
      keystr[34] = (char)l ? (char)l : ' ';
      put_str( keystr, 4*8 );
      to_base( sh, buf, 0, 16, 0 );
      put_str( buf, 13*8 );
      to_base( i, buf, 0, 16, 0 );
      put_str( buf, 24*8 );
      to_base( (char)l, buf, 0, 16, 0 );
      put_str( buf, 35*8 );
      continue;
    }
    if( i == '\x61' || (char)l == '\033' )
    {
#ifndef DEBUG
      if( sh&8 )
      {
        while( (kbshift)&0xf );
        keyhead=keytail;
        to_kbbuf();
      }
#endif
      done++;
    }
    else if( i == '\x47' )      /* Clr */
    {
      if( digits )
      {
        digits = 0;
        fname( old_fname );
      }
      else
      {
        fnc = fnc2 = 0L;
        fname("   ");
      }
      accum = 0;
      err = 0;
      display();
    }
    else if( err );
    else if( i>=0x54 && i<=0x5d )  /* Shift-fkey */
    {
      fkey[i-0x54] = accum;
      fkeys();
      digits=0;
      was_eq=1;
    }
    else if( i>=0x3b && i<=0x44 )  /* fkey */
    {
      accum = fkey[i-0x3b];
      display();
      digits=0;
      was_eq=1;
    }
    else if( i == 0xe && !was_eq && accum )
    {
      accum = accum / base;
      digits--;
      display();
    }
    else if( digits<width && 
        (ptr=strchr(hexstr,l)) != 0 && (i=ptr-hexstr) < base )
    {
      if( was_eq )
      {
        accum = i;
        was_eq = 0;
      }
      else if( accum*base < accum ) continue; /* check for overflow */
      else accum = accum*base+i;
      if( accum ) digits++;
      display();
    }
    else if( (char)l == '=' || (char)l == '\r' )
    {
      if( fnc )
      {
        if( (err=do_func(fnc)) != 0 ) continue;
        last = accum;
      }
      if( fnc2 )
      {
        last = last2;
        if( (err=do_func(fnc2)) != 0 ) continue;
      }
      fnc = fnc2 = 0L;
      fname("   ");
      digits = 0;
      was_eq=1;
    }
    else
      for( i=0; i<FUNCS; i++ )
        if( ((s=func[i].sh) < 0 && func[i].ch==(char)l) ||
            (s>=0 && s==sh && func[i].ch==(char)(l>>16)) )
        {
          if( (s=(*func[i].fn)(0)) == 1 )       /* binary op */
          {
            if( !was_eq && fnc )
              if( !is_highprec(fnc) && is_highprec(func[i].fn) )
              {
                fnc2 = fnc;
                last2 = last;
              }
              else if( (err=do_func(fnc)) != 0 ) break;
            if( fnc2 && !is_highprec(func[i].fn) )
            {
              last = last2;
              if( (err=do_func(fnc2)) != 0 ) break;
              fnc2 = 0L;
            }
            last = accum;
            fnc = func[i].fn;
            fname( func[i].name );
            digits=0;
            was_eq=1;
          }
          else if( s==2 )
          {
            err++;
            fname( errstr );
          }
          else if( s>2 )        /* unary op */
          {
            display();
            digits=0;
            was_eq=1;
          }
          else if( s<0 )        /* display change */
          {
            base = basetab[basen];
            width = widtab[basen][wordsize] + sign;
            wordsiz();
            display();
            fname( old_fname );
            digits=0;
            was_eq=1;
          }
          break;
        }
  }
  bot_line(1);
}

#ifdef DEBUG
char buffer[2000];
char in_auto=0;
#else
char *tpa;
long tpa_size;
#endif

int getcookie( long cookie, long *ptr )
{
    register long *cookiejar;
    long stack = Super(0L);

    if( (cookiejar = *(long **)0x5a0) == 0 )
    {
      Super((void *)stack);
      return 0;
    }
    do {
        if (*cookiejar == cookie)
        {
          if( ptr ) *ptr = *(cookiejar+1);
          Super((void *)stack);
          return 1;
        }
        else cookiejar += 2;
    } while (*cookiejar);
    Super((void *)stack);
    return 0;
}

main()
{
#ifdef DEBUG
  long stack;
#endif
  long l;
  
  linea0();
  scan_off = VWRAP;
  fht = V_CEL_MY >= 399 ? 16 : V_CEL_HT;
  if( getcookie( _VDO_COOKIE, &l ) ) falc_vid = *(int *)&l==3;
#ifdef DEBUG
  if( (disp = Malloc(disp_siz = (long)fht*scan_off)) == 0 ) return(-39);
  load_cfg( buffer, sizeof(buffer) );
  stack = Super((void *)0L);
  calc();
  Super( (void *)stack );
  return(0);
#else
  load_cfg( tpa, tpa_size );
  if( (disp_siz = (long)fht*scan_off) < user_disp ) disp_siz = user_disp;
  if( disp_siz > tpa_size )
  {
    Cconws( "Not enough memory for screen buffer!\r\n" );
    return(0);
  }
  disp = tpa;
  tpa+=disp_siz;
  tpa_size-=disp_siz;
  Cconws(INIT);
  return(1);
#endif
}
