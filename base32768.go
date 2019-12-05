package base32768

import (
	"sort"
	"strconv"
	"strings"
	"unicode/utf16"
)

/*
 * Encodings
 */

const blockBit = 5
const safeAlphabet = "ƀɀɠʀҠԀڀڠݠހ߀ကႠᄀᄠᅀᆀᇠሀሠበዠጠᎠᏀᐠᑀᑠᒀᒠᓀᓠᔀᔠᕀᕠᖀᖠᗀᗠᘀᘠᙀᚠᛀកᠠᡀᣀᦀ᧠ᨠᯀᰀᴀ⇠⋀⍀⍠⎀⎠⏀␀─┠╀╠▀■◀◠☀☠♀♠⚀⚠⛀⛠✀✠❀➀➠⠀⠠⡀⡠⢀⢠⣀⣠⤀⤠⥀⥠⦠⨠⩀⪀⪠⫠⬀⬠⭀ⰀⲀⲠⳀⴀⵀ⺠⻀㇀㐀㐠㑀㑠㒀㒠㓀㓠㔀㔠㕀㕠㖀㖠㗀㗠㘀㘠㙀㙠㚀㚠㛀㛠㜀㜠㝀㝠㞀㞠㟀㟠㠀㠠㡀㡠㢀㢠㣀㣠㤀㤠㥀㥠㦀㦠㧀㧠㨀㨠㩀㩠㪀㪠㫀㫠㬀㬠㭀㭠㮀㮠㯀㯠㰀㰠㱀㱠㲀㲠㳀㳠㴀㴠㵀㵠㶀㶠㷀㷠㸀㸠㹀㹠㺀㺠㻀㻠㼀㼠㽀㽠㾀㾠㿀㿠䀀䀠䁀䁠䂀䂠䃀䃠䄀䄠䅀䅠䆀䆠䇀䇠䈀䈠䉀䉠䊀䊠䋀䋠䌀䌠䍀䍠䎀䎠䏀䏠䐀䐠䑀䑠䒀䒠䓀䓠䔀䔠䕀䕠䖀䖠䗀䗠䘀䘠䙀䙠䚀䚠䛀䛠䜀䜠䝀䝠䞀䞠䟀䟠䠀䠠䡀䡠䢀䢠䣀䣠䤀䤠䥀䥠䦀䦠䧀䧠䨀䨠䩀䩠䪀䪠䫀䫠䬀䬠䭀䭠䮀䮠䯀䯠䰀䰠䱀䱠䲀䲠䳀䳠䴀䴠䵀䵠䶀䷀䷠一丠乀习亀亠什仠伀传佀你侀侠俀俠倀倠偀偠傀傠僀僠儀儠兀兠冀冠净几刀删剀剠劀加勀勠匀匠區占厀厠叀叠吀吠呀呠咀咠哀哠唀唠啀啠喀喠嗀嗠嘀嘠噀噠嚀嚠囀因圀圠址坠垀垠埀埠堀堠塀塠墀墠壀壠夀夠奀奠妀妠姀姠娀娠婀婠媀媠嫀嫠嬀嬠孀孠宀宠寀寠尀尠局屠岀岠峀峠崀崠嵀嵠嶀嶠巀巠帀帠幀幠庀庠廀廠开张彀彠往徠忀忠怀怠恀恠悀悠惀惠愀愠慀慠憀憠懀懠戀戠所扠技抠拀拠挀挠捀捠掀掠揀揠搀搠摀摠撀撠擀擠攀攠敀敠斀斠旀无昀映晀晠暀暠曀曠最朠杀杠枀枠柀柠栀栠桀桠梀梠检棠椀椠楀楠榀榠槀槠樀樠橀橠檀檠櫀櫠欀欠歀歠殀殠毀毠氀氠汀池沀沠泀泠洀洠浀浠涀涠淀淠渀渠湀湠満溠滀滠漀漠潀潠澀澠激濠瀀瀠灀灠炀炠烀烠焀焠煀煠熀熠燀燠爀爠牀牠犀犠狀狠猀猠獀獠玀玠珀珠琀琠瑀瑠璀璠瓀瓠甀甠畀畠疀疠痀痠瘀瘠癀癠皀皠盀盠眀眠着睠瞀瞠矀矠砀砠础硠碀碠磀磠礀礠祀祠禀禠秀秠稀稠穀穠窀窠竀章笀笠筀筠简箠節篠簀簠籀籠粀粠糀糠紀素絀絠綀綠緀締縀縠繀繠纀纠绀绠缀缠罀罠羀羠翀翠耀耠聀聠肀肠胀胠脀脠腀腠膀膠臀臠舀舠艀艠芀芠苀苠茀茠荀荠莀莠菀菠萀萠葀葠蒀蒠蓀蓠蔀蔠蕀蕠薀薠藀藠蘀蘠虀虠蚀蚠蛀蛠蜀蜠蝀蝠螀螠蟀蟠蠀蠠血衠袀袠裀裠褀褠襀襠覀覠觀觠言訠詀詠誀誠諀諠謀謠譀譠讀讠诀诠谀谠豀豠貀負賀賠贀贠赀赠趀趠跀跠踀踠蹀蹠躀躠軀軠輀輠轀轠辀辠迀迠退造遀遠邀邠郀郠鄀鄠酀酠醀醠釀釠鈀鈠鉀鉠銀銠鋀鋠錀錠鍀鍠鎀鎠鏀鏠鐀鐠鑀鑠钀钠铀铠销锠镀镠門閠闀闠阀阠陀陠隀隠雀雠需霠靀靠鞀鞠韀韠頀頠顀顠颀颠飀飠餀餠饀饠馀馠駀駠騀騠驀驠骀骠髀髠鬀鬠魀魠鮀鮠鯀鯠鰀鰠鱀鱠鲀鲠鳀鳠鴀鴠鵀鵠鶀鶠鷀鷠鸀鸠鹀鹠麀麠黀黠鼀鼠齀齠龀龠ꀀꀠꁀꁠꂀꂠꃀꃠꄀꄠꅀꅠꆀꆠꇀꇠꈀꈠꉀꉠꊀꊠꋀꋠꌀꌠꍀꍠꎀꎠꏀꏠꐀꐠꑀꑠ꒠ꔀꔠꕀꕠꖀꖠꗀꗠꙀꚠꛀ꜀꜠ꝀꞀꡀ"
const shortAlphabet = "ÀàĀĠŀŠƀƠǀǠȀȠɀɠʀʠˀˠ̠̀̀͠΀ΠπϠЀРрѠҀҠӀӠԀԠՀՠր֠׀נ؀ؠـ٠ڀڠۀ۠܀ܠ݀ݠހޠ߀ߠዠጠᎠᏀᐠᑀᑠᒀᒠᓀᓠᔀᔠᕀᕠᖀᖠᗀᗠᘀᘠᙀᚠᛀកᠠᡀᣀ─┠╀╠▀■◀◠☀☠♀♠⚀⚠⛀⛠✀✠❀➀➠⠠⡀⡠⢀⢠⣀⣠⫠⬀⬠⭀ⰀⲀⲠⳀⴀⵀ㐀㐠㑀㑠㒀㒠㓀㓠㔀㔠㕀㕠㖀㖠㗀㗠㘀㘠㙀㙠㚀㚠㛀㛠㜀㜠㝀㝠㞀㞠㟀㟠㠀㠠㡀㡠㢀㢠㣀㣠㤀㤠㥀㥠㦀㦠㧀㧠㨀㨠㩀㩠㪀㪠㫀㫠㬀㬠㭀㭠㮀㮠㯀㯠㰀㰠㱀㱠㲀㲠㳀㳠㴀㴠㵀㵠㶀㶠㷀㷠㸀㸠㹀㹠㺀㺠㻀㻠㼀㼠㽀㽠㾀㾠㿀㿠䀀䀠䁀䁠䂀䂠䃀䃠䄀䄠䅀䅠䆀䆠䇀䇠䈀䈠䉀䉠䊀䊠䋀䋠䌀䌠䍀䍠䎀䎠䏀䏠䐀䐠䑀䑠䒀䒠䓀䓠䔀䔠䕀䕠䖀䖠䗀䗠䘀䘠䙀䙠䚀䚠䛀䛠䜀䜠䝀䝠䞀䞠䟀䟠䠀䠠䡀䡠䢀䢠䣀䣠䤀䤠䥀䥠䦀䦠䧀䧠䨀䨠䩀䩠䪀䪠䫀䫠䬀䬠䭀䭠䮀䮠䯀䯠䰀䰠䱀䱠䲀䲠䳀䳠䴀䴠䵀䵠䶀䷀䷠一丠乀习亀亠什仠伀传佀你侀侠俀俠倀倠偀偠傀傠僀僠儀儠兀兠冀冠净几刀删剀剠劀加勀勠匀匠區占厀厠叀叠吀吠呀呠咀咠哀哠唀唠啀啠喀喠嗀嗠嘀嘠噀噠嚀嚠囀因圀圠址坠垀垠埀埠堀堠塀塠墀墠壀壠夀夠奀奠妀妠姀姠娀娠婀婠媀媠嫀嫠嬀嬠孀孠宀宠寀寠尀尠局屠岀岠峀峠崀崠嵀嵠嶀嶠巀巠帀帠幀幠庀庠廀廠开张彀彠往徠忀忠怀怠恀恠悀悠惀惠愀愠慀慠憀憠懀懠戀戠所扠技抠拀拠挀挠捀捠掀掠揀揠搀搠摀摠撀撠擀擠攀攠敀敠斀斠旀无昀映晀晠暀暠曀曠最朠杀杠枀枠柀柠栀栠桀桠梀梠检棠椀椠楀楠榀榠槀槠樀樠橀橠檀檠櫀櫠欀欠歀歠殀殠毀毠氀氠汀池沀沠泀泠洀洠浀浠涀涠淀淠渀渠湀湠満溠滀滠漀漠潀潠澀澠激濠瀀瀠灀灠炀炠烀烠焀焠煀煠熀熠燀燠爀爠牀牠犀犠狀狠猀猠獀獠玀玠珀珠琀琠瑀瑠璀璠瓀瓠甀甠畀畠疀疠痀痠瘀瘠癀癠皀皠盀盠眀眠着睠瞀瞠矀矠砀砠础硠碀碠磀磠礀礠祀祠禀禠秀秠稀稠穀穠窀窠竀章笀笠筀筠简箠節篠簀簠籀籠粀粠糀糠紀素絀絠綀綠緀締縀縠繀繠纀纠绀绠缀缠罀罠羀羠翀翠耀耠聀聠肀肠胀胠脀脠腀腠膀膠臀臠舀舠艀艠芀芠苀苠茀茠荀荠莀莠菀菠萀萠葀葠蒀蒠蓀蓠蔀蔠蕀蕠薀薠藀藠蘀蘠虀虠蚀蚠蛀蛠蜀蜠蝀蝠螀螠蟀蟠蠀蠠血衠袀袠裀裠褀褠襀襠覀覠觀觠言訠詀詠誀誠諀諠謀謠譀譠讀讠诀诠谀谠豀豠貀負賀賠贀贠赀赠趀趠跀跠踀踠蹀蹠躀躠軀軠輀輠轀轠辀辠迀迠退造遀遠邀邠郀郠鄀鄠酀酠醀醠釀釠鈀鈠鉀鉠銀銠鋀鋠錀錠鍀鍠鎀鎠鏀鏠鐀鐠鑀鑠钀钠铀铠销锠镀镠門閠闀闠阀阠陀陠隀隠雀雠需霠靀靠鞀鞠韀韠頀頠顀顠颀颠飀飠餀餠饀饠馀馠駀駠騀騠驀驠骀骠髀髠鬀鬠魀魠鮀鮠鯀鯠鰀鰠鱀鱠鲀鲠鳀鳠鴀鴠鵀鵠鶀鶠鷀鷠鸀鸠鹀鹠麀麠黀黠鼀鼠齀齠龀龠ꀀꀠꁀꁠꂀꂠꃀꃠꄀꄠꅀꅠꆀꆠꇀꇠꈀꈠꉀꉠꊀꊠꋀꋠꌀꌠꍀꍠꎀꎠꏀꏠꐀꐠꑀꑠ꒠ꔀꔠꕀꕠꖀꖠ"

// An Encoding is a radix 32768 encoding/decoding scheme, defined by a
// 32768-character alphabet.
type Encoding struct {
	encodeA   [1024]uint16
	encodeB   [4]uint16
	decodeMap [2048]uint16
	splitter  uint16
}

// NewEncoding returns a new Encoding defined by the given alphabet,
// The alphabet must be a 1028 characters long and contains only BMP
// character and 32 block leading characters.
func NewEncoding(encoder string) *Encoding {
	e := new(Encoding)
	encode := make([]uint16, 1028)
	i := 0
	for _, r := range encoder {
		if r&0xFFE0 != r {
			panic("encoding alphabet containing illegal character")
		}
		if i >= len(encode) {
			break
		}
		encode[i] = uint16(r)
		i++
	}
	if i < len(encode) {
		panic("encoding alphabet is not 1028-characters long")
	}
	sort.Slice(encode, func(i, j int) bool { return encode[i] < encode[j] })
	e.splitter = encode[4]
	copy(e.encodeA[:], encode[4:])
	copy(e.encodeB[:], encode[:4])

	for i := 0; i < len(e.decodeMap); i++ {
		e.decodeMap[i] = 0xFFFD
	}
	for i := 0; i < len(e.encodeA); i++ {
		idx := e.encodeA[i] >> blockBit
		if e.decodeMap[idx] != 0xFFFD {
			panic("encoding alphabet have repeating character")
		}
		e.decodeMap[idx] = uint16(i) << blockBit
	}
	for i := 0; i < len(e.encodeB); i++ {
		idx := e.encodeB[i] >> blockBit
		if e.decodeMap[idx] != 0xFFFD {
			panic("encoding alphabet have repeating character")
		}
		e.decodeMap[idx] = uint16(i) << blockBit
	}
	return e
}

// SafeEncoding is a base32768 encoding using only "safe"
// Unicode runes
var SafeEncoding = NewEncoding(safeAlphabet)
// ShortEncoding is a base32768 encoding use all codepoint
// from 0x00C0 - 0x0800 to optimize UTF-8 behavior
var ShortEncoding = NewEncoding(shortAlphabet)

var removeNewlinesMapper = func(r rune) rune {
	if r == '\r' || r == '\n' {
		return -1
	}
	return r
}

func (enc *Encoding) encode15(src uint16) uint16 {
	src = src & 0x7FFF
	dst := enc.encodeA[src>>blockBit]
	dst |= uint16(src & (1<<blockBit - 1))
	return dst
}

func (enc *Encoding) encode7(src byte) uint16 {
	src = src & 0x7F
	dst := enc.encodeB[src>>blockBit]
	dst |= uint16(src & (1<<blockBit - 1))
	return dst
}

func (enc *Encoding) decode(src uint16) (uint16, bool, bool) {
	isTrailing := src < enc.splitter
	dst := enc.decodeMap[src>>blockBit]
	if dst == 0xFFFD {
		return dst, isTrailing, false
	}
	dst |= src & (1<<blockBit - 1)
	return dst, isTrailing, true
}

func (enc *Encoding) encodeUint16(dst []uint16, src []byte) {
	var left byte
	var leftn uint8
	for len(src) > 0 {
		var chunk uint16 // Chunk contains 15 bits
		chunk = uint16(left) << (15 - leftn)
		chunk |= uint16(src[0]) << (7 - leftn)
		if leftn < 7 && len(src) > 1 {
			chunk |= uint16(src[1]) >> (1 + leftn)
			left = src[1] & (1<<(1+leftn) - 1)
			leftn++
			src = src[2:] // 2 bytes taken
		} else {
			chunk |= 1<<(7-leftn) - 1 // Pad with 1s
			left = 0
			leftn = 0
			src = src[1:] // 1 byte taken
		}
		dst[0] = enc.encode15(chunk)
		dst = dst[1:]
	}
	// Remaining
	if leftn > 0 {
		left = left << (7 - leftn)
		left |= 1<<(7-leftn) - 1 // Pad with 1s
		dst[0] = enc.encode7(left)
	}
}

// Encode encodes src using the encoding enc, writing
// EncodedLen(len(src)) bytes to dst.
func (enc *Encoding) Encode(dst, src []byte) {
	buf := make([]uint16, enc.EncodedLen(len(src))/2)
	enc.encodeUint16(buf, src)
	for _, b := range buf {
		if len(dst) > 1 {
			dst[0] = byte(b >> 8)
			dst[1] = byte(b)
			dst = dst[2:]
		}
	}
}

// EncodeToString returns the base32768 encoding of src.
func (enc *Encoding) EncodeToString(src []byte) string {
	buf := make([]uint16, enc.EncodedLen(len(src))/2)
	enc.encodeUint16(buf, src)
	return string(utf16.Decode(buf))
}

// EncodedLen returns the length in bytes of the base32768 encoding
// of an input buffer of length n.
func (enc *Encoding) EncodedLen(n int) int {
	return (8*n + 14) / 15 * 2
}

type CorruptInputError int64

func (e CorruptInputError) Error() string {
	return "illegal base32768 data at input byte " + strconv.FormatInt(int64(e*2), 10)
}

func (enc *Encoding) decodeUint16(dst []byte, src []uint16) (n int, err error) {
	olen := len(src)
	var left byte
	var leftn uint8
	for len(src) > 0 {
		chunk := src[0]
		d, trailing, success := enc.decode(chunk)
		if !success {
			return n, CorruptInputError(olen - len(src))
		}
		if trailing {
			// Left one byte
			if leftn > 0 {
				dst[0] = left<<(8-leftn) | byte(d>>(leftn-1))
				n++
			}
			return n, nil
		}
		// Read 15 bits
		if leftn > 0 {
			dst[0] = left<<(8-leftn) | byte(d>>(7+leftn))
			dst[1] = byte(d >> (leftn - 1))
			left = byte(d) & (1<<(leftn-1) - 1)
			leftn--
			dst = dst[2:]
			n += 2
		} else {
			dst[0] = byte(d >> 7)
			left = byte(d) & 0x7F
			leftn = 7
			dst = dst[1:]
			n++
		}
		src = src[1:]
	}
	return n, nil
}

// Decode decodes src using the encoding enc. It writes at most
// DecodedLen(len(src)) bytes to dst and returns the number of bytes
// written. If src contains invalid base32768 data, it will return the
// number of bytes successfully written and CorruptInputError.
// New line characters (\r and \n) are ignored.
func (enc *Encoding) Decode(dst, src []byte) (n int, err error) {
	if len(src)%2 != 0 {
		return 0, CorruptInputError(0)
	}
	buf := make([]uint16, len(src)/2)
	for i := range buf {
		if len(src) > 1 {
			buf[i] = uint16(src[0])<<8 | uint16(src[1])
			src = src[2:]
		}
	}
	return enc.decodeUint16(dst, buf)
}

// DecodeString returns the bytes represented by the base32768 string s.
func (enc *Encoding) DecodeString(s string) ([]byte, error) {
	s = strings.Map(removeNewlinesMapper, s)
	src := utf16.Encode([]rune(s))
	dbuf := make([]byte, enc.DecodedLen(len(src)*2))
	n, err := enc.decodeUint16(dbuf, src)
	return dbuf[:n], err
}

// DecodedLen returns the maximum length in bytes of the decoded data
// corresponding to n bytes of base32768-encoded data.
func (enc *Encoding) DecodedLen(n int) int {
	return n / 2 * 15 / 8
}
