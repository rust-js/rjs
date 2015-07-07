extern crate chrono;
extern crate time;

use ::{JsResult, JsError};
use rt::{JsEnv, JsArgs, JsValue, JsFnMode, JsPreferredType, JsType, JsString, JsItem, JsHandle};
use syntax::token::name;
use gc::{self, AsPtr};
use std::f64;
use self::chrono::*;
use std::fmt::Write;

struct TimeZoneInfo {
    offset: i32,
    dst: bool
}

impl TimeZoneInfo {
    fn new() -> TimeZoneInfo {
        let tm = self::time::now();
        
        TimeZoneInfo {
            offset: tm.tm_utcoff,
            dst: tm.tm_isdst > 0
        }
    }
}

// 15.9.1.1 Time Values and Time Range

const MS_PER_DAY : f64 = 86_400_000.0;
const HOURS_PER_DAY : f64 = 24.0;
const MINUTES_PER_HOUR : f64 = 60.0;
const SECONDS_PER_MINUTE : f64 = 60.0;
const MS_PER_SECOND : f64 = 1000.0;
const MS_PER_MINUTE : f64 = 60_000.0;
const MS_PER_HOUR : f64 = 3_600_000.0;

// 15.9.1.2 Day Number and Time within Day
fn get_day(time: f64) -> f64 {
    (time / MS_PER_DAY).floor()
}

// 15.9.1.2 Day Number and Time within Day
fn get_time_within_day(time: f64) -> f64 {
    time % MS_PER_DAY
}

// 15.9.1.3 Year Number

// DaysInYear(y) = 365 if (y modulo 4) ≠ 0  
// = 366 if (y modulo 4) = 0 and (y modulo 100) ≠ 0  
// = 365 if (y modulo 100) = 0 and (y modulo 400) ≠ 0  
// = 366 if (y modulo 400) = 0
fn get_days_in_year(year: f64) -> f64 {
    if year % 4.0 != 0.0 {
        365.0
    } else if year % 100.0 != 0.0 {
        366.0
    } else if year % 400.0 != 0.0 {
        365.0
    } else {
        366.0
    }
}
// 
// DayFromYear(y) = 365 × (y−1970) + floor((y−1969)/4) − floor((y−1901)/100) + floor((y−1601)/400)
fn get_day_from_year(year: f64) -> f64 {
    365.0 * (year - 1970.0) +
        ((year - 1969.0) / 4.0).floor() -
        ((year - 1901.0) / 100.0).floor() +
        ((year - 1601.0) / 400.0).floor()
}

// YearFromTime(t) = the largest integer y (closest to positive infinity) such that TimeFromYear(y) ≤ t
fn get_year_from_time(time: f64) -> f64 {
    if let Some(date_time) = time_to_chrono(time) {
        date_time.year() as f64
    } else {
        // TODO #40: This is wrong. Chrono should have been able to parse
        // this in the firs tplace.
        f64::NAN
    }
}

// InLeapYear(t) = 0 if DaysInYear(YearFromTime(t)) = 365  
// = 1 if DaysInYear(YearFromTime(t)) = 366
fn get_in_leap_year(time: f64) -> f64 {
    if get_days_in_year(get_year_from_time(time)) == 365.0 {
        0.0
    } else {
        1.0
    }
}

// 15.9.1.4 Month Number

// MonthFromTime(t) = 0 if 0 ≤ DayWithinYear(t) < 31  
//  = 1 if 31 ≤ DayWithinYear (t) < 59+InLeapYear(t)  
//  = 2 if 59+InLeapYear(t) ≤ DayWithinYear (t) < 90+InLeapYear(t)  
//  = 3 if 90+InLeapYear(t) ≤ DayWithinYear (t) < 120+InLeapYear(t)  
//  = 4 if 120+InLeapYear(t) ≤ DayWithinYear (t) < 151+InLeapYear(t)  
//  = 5 if 151+InLeapYear(t) ≤ DayWithinYear (t) < 181+InLeapYear(t)  
//  = 6 if 181+InLeapYear(t) ≤ DayWithinYear (t) < 212+InLeapYear(t)  
//  = 7 if 212+InLeapYear(t) ≤ DayWithinYear (t) < 243+InLeapYear(t)  
//  = 8 if 243+InLeapYear(t) ≤ DayWithinYear (t) < 273+InLeapYear(t)  
//  = 9 if 273+InLeapYear(t) ≤ DayWithinYear (t) < 304+InLeapYear(t)  
//  = 10 if 304+InLeapYear(t) ≤ DayWithinYear (t) < 334+InLeapYear(t)  
//  = 11 if 334+InLeapYear(t) ≤ DayWithinYear (t) < 365+InLeapYear(t)
fn get_month_from_time(time: f64) -> f64 {
    let day_within_year = get_day_within_year(time);
    let in_leap_year = get_in_leap_year(time);
    
    if day_within_year < 31.0 {
        0.0
    } else if day_within_year < 59.0 + in_leap_year {
        1.0
    } else if day_within_year < 90.0 + in_leap_year {
        2.0
    } else if day_within_year < 120.0 + in_leap_year {
        3.0
    } else if day_within_year < 151.0 + in_leap_year {
        4.0
    } else if day_within_year < 181.0 + in_leap_year {
        5.0
    } else if day_within_year < 212.0 + in_leap_year {
        6.0
    } else if day_within_year < 243.0 + in_leap_year {
        7.0
    } else if day_within_year < 273.0 + in_leap_year {
        8.0
    } else if day_within_year < 304.0 + in_leap_year {
        9.0
    } else if day_within_year < 334.0 + in_leap_year {
        10.0
    } else {
        11.0
    }
}

// DayWithinYear(t) = Day(t)−DayFromYear(YearFromTime(t))
fn get_day_within_year(time: f64) -> f64 {
    get_day(time) - get_day_from_year(get_year_from_time(time))
}

// 15.9.1.5 Date Number

// DateFromTime(t) = DayWithinYear(t)+1 if MonthFromTime(t)=0  
//  = DayWithinYear(t)−30 if MonthFromTime(t)=1  
//  = DayWithinYear(t)−58−InLeapYear(t) if MonthFromTime(t)=2  
//  = DayWithinYear(t)−89−InLeapYear(t) if MonthFromTime(t)=3  
//  = DayWithinYear(t)−119−InLeapYear(t) if MonthFromTime(t)=4  
//  = DayWithinYear(t)−150−InLeapYear(t) if MonthFromTime(t)=5  
//  = DayWithinYear(t)−180−InLeapYear(t) if MonthFromTime(t)=6  
//  = DayWithinYear(t)−211−InLeapYear(t) if MonthFromTime(t)=7  
//  = DayWithinYear(t)−242−InLeapYear(t) if MonthFromTime(t)=8  
//  = DayWithinYear(t)−272−InLeapYear(t) if MonthFromTime(t)=9  
//  = DayWithinYear(t)−303−InLeapYear(t) if MonthFromTime(t)=10  
//  = DayWithinYear(t)−333−InLeapYear(t) if MonthFromTime(t)=11
fn get_date_from_time(time: f64) -> f64 {
    let day_within_year = get_day_within_year(time);
    let in_leap_year = get_in_leap_year(time);
    
    match get_month_from_time(time) {
        0.0 => day_within_year + 1.0,
        1.0 => day_within_year - 30.0,
        2.0 => day_within_year - 58.0 - in_leap_year,
        3.0 => day_within_year - 89.0 - in_leap_year,
        4.0 => day_within_year - 119.0 - in_leap_year,
        5.0 => day_within_year - 150.0 - in_leap_year,
        6.0 => day_within_year - 180.0 - in_leap_year,
        7.0 => day_within_year - 211.0 - in_leap_year,
        8.0 => day_within_year - 242.0 - in_leap_year,
        9.0 => day_within_year - 272.0 - in_leap_year,
        10.0 => day_within_year - 303.0 - in_leap_year,
        11.0 => day_within_year - 333.0 - in_leap_year,
        _ => panic!("unexpected month")
    }
}

// 15.9.1.6 Week Day

// WeekDay(t) = (Day(t) + 4) modulo 7
fn get_week_day(time: f64) -> f64 {
    (get_day(time) + 4.0) % 7.0
}

// 15.9.1.7 Local Time Zone Adjustment

// An implementation of ECMAScript is expected to determine the local time zone adjustment. The local
// time zone adjustment is a value LocalTZA measured in milliseconds which when added to UTC represents
// the local standard time. Daylight saving time is not reflected by LocalTZA. The value LocalTZA does
// not vary with time but depends only on the geographic location.
fn get_local_tza() -> f64 {
    TimeZoneInfo::new().offset as f64 * MS_PER_SECOND
}

// 15.9.1.8 Daylight Saving Time Adjustment
fn get_daylight_saving_ta(_time: f64) -> f64 {
    match TimeZoneInfo::new().dst {
        true => MS_PER_HOUR,
        false => 0.0
    }
}

// 15.9.1.9 Local Time

// LocalTime(t) = t + LocalTZA + DaylightSavingTA(t)
fn get_local_time(time: f64) -> f64 {
    time + get_local_tza() + get_daylight_saving_ta(time)
}

// UTC(t) = t – LocalTZA – DaylightSavingTA(t – LocalTZA)
fn get_utc(time: f64) -> f64 {
    let local_tza = get_local_tza();
    
    time - local_tza - get_daylight_saving_ta(time - local_tza)
}

// 15.9.1.10 Hours, Minutes, Second, and Milliseconds

fn positive_mod(value: f64, divisor: f64) -> f64 {
    let result = value % divisor;
    if result < 0.0 {
        result + divisor
    } else {
        result
    }
}

// HourFromTime(t) = floor(t / msPerHour) modulo HoursPerDay
fn get_hour_from_time(time: f64) -> f64 {
    positive_mod((time / MS_PER_HOUR).floor(), HOURS_PER_DAY)
}

// MinFromTime(t) = floor(t / msPerMinute) modulo MinutesPerHour
fn get_min_from_time(time: f64) -> f64 {
    positive_mod((time / MS_PER_MINUTE).floor(), MINUTES_PER_HOUR)
}

// SecFromTime(t) = floor(t / msPerSecond) modulo SecondsPerMinute
fn get_sec_from_time(time: f64) -> f64 {
    positive_mod((time / MS_PER_SECOND).floor(), SECONDS_PER_MINUTE)
}

// msFromTime(t) = t modulo msPerSecond
fn get_ms_from_time(time: f64) -> f64 {
    positive_mod(time, MS_PER_SECOND)
}

// 15.9.1.11 MakeTime (hour, min, sec, ms)
fn make_time(hour: f64, min: f64, sec: f64, ms: f64) -> f64 {
    if !hour.is_finite() || !min.is_finite() || !sec.is_finite() || !ms.is_finite() {
        f64::NAN
    } else {
        let hour = hour.trunc();
        let min = min.trunc();
        let sec = sec.trunc();
        let ms = ms.trunc();
        
        hour * MS_PER_HOUR + min * MS_PER_MINUTE + sec * MS_PER_SECOND + ms
    }
}

// 15.9.1.12 MakeDay (year, month, date)
fn make_day(year: f64, month: f64, date: f64) -> f64 {
    if !year.is_finite() || !month.is_finite() || !date.is_finite() {
        f64::NAN
    } else {
        let year = year.trunc();
        let month = month.trunc();
        let date = date.trunc();
        
        let time = match UTC.ymd_opt(year as i32, month as u32 + 1, 1) {
            LocalResult::Single(date) => time_from_chrono(date.and_hms(0, 0, 0)),
            _ => f64::NAN
        };
        
        get_day(time) + date - 1.0
    }
}

// 15.9.1.13 MakeDate (day, time)
fn make_date(day: f64, time: f64) -> f64 {
    if !day.is_finite() || !time.is_finite () {
        f64::NAN
    } else {
        day * MS_PER_DAY + time
    }
}

// 15.9.1.14 TimeClip (time)
fn time_clip(time: f64) -> f64 {
    if !time.is_finite() {
        f64::NAN
    } else if time.abs() > 8.64e15 {
        f64::NAN
    } else {
        time.trunc()
    }
}

// 15.9.1.15 Date Time String Format
// 15.9.1.15.1 Extended years
// ==========================
// 
// ECMAScript requires the ability to specify 6 digit years (extended years); approximately 285,426
// years, either forward or backward, from 01 January, 1970 UTC. To represent years before 0 or after
// 9999, ISO 8601 permits the expansion of the year representation, but only by prior agreement between
// the sender and the receiver. In the simplified ECMAScript format such an expanded year
// representation shall have 2 extra year digits and is always prefixed with a + or – sign. The year 0
// is considered positive and hence prefixed with a + sign.
// 
// NOTE Examples of extended years:
// 
// -283457-03-21T15:00:59.008Z   283458 B.C.  
// -000001-01-01T00:00:00Z          2 B.C.  
// +000000-01-01T00:00:00Z         1 B.C.  
// +000001-01-01T00:00:00Z         1 A.D.  
// +001970-01-01T00:00:00Z         1970 A.D.  
// +002009-12-15T00:00:00Z         2009 A.D.  
// +287396-10-12T08:59:00.992Z 287396 A.D.
fn parse(string: &str) -> f64 {
    let mut year = 0;
    let mut month = 0;
    let mut date = 0;
    let mut hours = 0;
    let mut minutes = 0;
    let mut seconds = 0;
    let mut ms = 0;
    let mut tz_sign = 1;
    let mut tz_hours = 0;
    let mut tz_minutes = 0;
    let mut tz_allow = false;
    
    let mut index = 0;
    
    for c in string.chars() {
        match index {
            0...3 if c.is_digit(10) => {
                year *= 10;
                year += c.to_digit(10).unwrap();
            }
            4 if c == '-' => {},
            5...6 if c.is_digit(10) => {
                month *= 10;
                month += c.to_digit(10).unwrap();
            }
            7 if c == '-' => {},
            8...9 if c.is_digit(10) => {
                date *= 10;
                date += c.to_digit(10).unwrap();
            }
            10 if c == 'T' => {},
            11...12 if c.is_digit(10) => {
                hours *= 10;
                hours += c.to_digit(10).unwrap();
            }
            13 if c == ':' => {},
            14...15 if c.is_digit(10) => {
                minutes *= 10;
                minutes += c.to_digit(10).unwrap();
            }
            16 if c == ':' => {},
            17...18 if c.is_digit(10) => {
                seconds *= 10;
                seconds += c.to_digit(10).unwrap();
            }
            19 if c == '.' => {},
            20...21 if c.is_digit(10) => {
                ms *= 10;
                ms += c.to_digit(10).unwrap();
            }
            22 if c == 'Z' => {},
            22 if c == '+' => {
                tz_allow = true;
                tz_sign = 1;
            }
            22 if c == '-' => {
                tz_allow = true;
                tz_sign = -1;
            }
            23...24 if tz_allow && c.is_digit(10) => {
                tz_hours *= 10;
                tz_hours += c.to_digit(10).unwrap();
            }
            25 if tz_allow && c == ':' => {},
            26...27 if tz_allow && c.is_digit(10) => {
                tz_minutes *= 10;
                tz_minutes += c.to_digit(10).unwrap();
            }
            _ => return f64::NAN
        }
        
        index += 1;
    }
    
    if month != 0 {
        month -= 1;
    }
    if date == 0 {
        date = 1;
    }
    
    let time = make_date(
        make_day(year as f64, month as f64, date as f64),
        make_time(hours as f64, minutes as f64, seconds as f64, ms as f64)
    );
    
    let tza = tz_sign as f64 * (tz_hours as f64 * MS_PER_HOUR + tz_minutes as f64 * MS_PER_MINUTE);
    
    time - tza
}

// 15.9.2 The Date Constructor Called as a Function
// 15.9.3 The Date Constructor
// 15.9.2.1 Date ( [ year [, month [, date [, hours [, minutes [, seconds [, ms ] ] ] ] ] ] ] )
// 15.9.3.1 new Date (year, month [, date [, hours [, minutes [, seconds [, ms ] ] ] ] ] )
// 15.9.3.2 new Date (value)
// 15.9.3.3 new Date ( )
pub fn Date_constructor(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    if !mode.construct() {
        let result = try!(args.function(env).construct(env, Vec::new()));
        return Ok(try!(result.to_string(env)).as_value(env));
    }
    
    let time = if args.argc == 0 {
        get_utc_now()
    } else if args.argc == 1 {
        let arg = try!(args.arg(env, 0).to_primitive(env, JsPreferredType::None));
        
        match arg.ty() {
            JsType::String => parse(&arg.unwrap_string(env).to_string()),
            _ => try!(arg.to_number(env))
        }
    } else {
        get_utc(try!(make_date_from_args(env, &args)))
    };
    
    let this = args.this(env);
    
    let mut this_obj = this.unwrap_object(env);
    
    this_obj.set_class(env, Some(name::DATE_CLASS));
    this_obj.set_value(env.new_number(time_clip(time)));
    
    Ok(this)
}

fn time_to_chrono(time: f64) -> Option<DateTime<UTC>> {
    let ms = get_ms_from_time(time);
    
    if let Some(time) = NaiveDateTime::from_timestamp_opt((time / 1_000.0) as i64, 0) {
        DateTime::<UTC>::from_utc(time, UTC).with_nanosecond((ms * 1_000_000.0) as u32)
    } else {
        None
    }
}

fn time_from_chrono(date_time: DateTime<UTC>) -> f64 {
    let timestamp = date_time.naive_utc().timestamp() as f64 * 1_000.0;
    timestamp + (date_time.nanosecond() as f64 / 1_000_000.0).trunc()
}

fn get_utc_now() -> f64 {
    time_from_chrono(UTC::now())
}

fn make_date_from_args(env: &mut JsEnv, args: &JsArgs) -> JsResult<f64> {
    let year = try!(args.arg(env, 0).to_number(env));
    let month = try!(args.arg(env, 1).to_number(env));
    let date = try!(args.map_or(env, 2, Ok(1.0), |env, p| p.to_number(env)));
    let hours = try!(args.map_or(env, 3, Ok(0.0), |env, p| p.to_number(env)));
    let minutes = try!(args.map_or(env, 4, Ok(0.0), |env, p| p.to_number(env)));
    let seconds = try!(args.map_or(env, 5, Ok(0.0), |env, p| p.to_number(env)));
    let ms = try!(args.map_or(env, 6, Ok(0.0), |env, p| p.to_number(env)));
    
    let year = if !year.is_nan() {
        let int_year = year.trunc();
        if year >= 0.0 && year <= 99.0 {
            1900.0 + int_year
        } else {
            year
        }
    } else {
        year
    };
    
    Ok(make_date(make_day(year, month, date), make_time(hours, minutes, seconds, ms)))
}

fn get_time_from_args(env: &mut JsEnv, args: &JsArgs) -> JsResult<f64> {
    let this = args.this(env);
    
    if this.ty() == JsType::Object {
        let this = this.unwrap_object(env);
        if let Some(prototype) = this.prototype(env) {
            if
                prototype.ty() == JsType::Object &&
                prototype.unwrap_object(env).as_ptr() == env.handle(JsHandle::Date).as_ptr()
            {
                return Ok(this.value(env).unwrap_number());
            }
        }
    }
    
    Err(JsError::new_type(env, ::errors::TYPE_NOT_DATE))
}

// 15.9.4.2 Date.parse (string)
pub fn Date_parse(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let string = try!(args.arg(env, 0).to_string(env)).to_string();
    
    let time = parse(&string);
    let time = env.new_number(time);
    
    let constructor = try!(env.handle(JsHandle::Date).get(env, name::CONSTRUCTOR));
    let date = try!(constructor.construct(env, vec![time]));
    
    Ok(date.as_value(env))
}

// 15.9.4.3 Date.UTC (year, month [, date [, hours [, minutes [, seconds [, ms ] ] ] ] ] )
pub fn Date_UTC(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(make_date_from_args(env, &args));
    
    Ok(env.new_number(time_clip(time)))
}

// 15.9.4.4 Date.now ( )
pub fn Date_now(env: &mut JsEnv, _mode: JsFnMode, _args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    Ok(env.new_number(get_utc_now()))
}

// 15.9.5.2 Date.prototype.toString ( )
pub fn Date_toString(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    
    let mut result = String::new();
    
    // TODO #40: Incorporate time zone.
    
    write!(
        result,
        "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}.{:03}Z",
        get_year_from_time(time) as u32,
        get_month_from_time(time) as u32 + 1,
        get_date_from_time(time) as u32,
        get_hour_from_time(time) as u32,
        get_min_from_time(time) as u32,
        get_sec_from_time(time) as u32,
        get_ms_from_time(time) as u32
    ).ok();
    
    Ok(JsString::from_str(env, &result).as_value(env))
}

// 15.9.5.3 Date.prototype.toDateString ( )
pub fn Date_toDateString(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    
    let mut result = String::new();
    
    // TODO #40: Incorporate time zone.
    
    write!(
        result,
        "{:04}-{:02}-{:02}",
        get_year_from_time(time) as u32,
        get_month_from_time(time) as u32 + 1,
        get_date_from_time(time) as u32
    ).ok();
    
    Ok(JsString::from_str(env, &result).as_value(env))
}

// 15.9.5.4 Date.prototype.toTimeString ( )
pub fn Date_toTimeString(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    
    let mut result = String::new();
    
    // TODO #40: Incorporate time zone.
    
    write!(
        result,
        "{:02}:{:02}:{:02}.{:03}",
        get_hour_from_time(time) as u32,
        get_min_from_time(time) as u32,
        get_sec_from_time(time) as u32,
        get_ms_from_time(time) as u32
    ).ok();
    
    Ok(JsString::from_str(env, &result).as_value(env))
}

// 15.9.5.5 Date.prototype.toLocaleString ( )
pub fn Date_toLocaleString(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    Date_toString(env, mode, args)
}

// 15.9.5.6 Date.prototype.toLocaleDateString ( )
pub fn Date_toLocaleDateString(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    Date_toDateString(env, mode, args)
}

// 15.9.5.7 Date.prototype.toLocaleTimeString ( )
pub fn Date_toLocaleTimeString(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    Date_toTimeString(env, mode, args)
}

// 15.9.5.8 Date.prototype.valueOf ( )
pub fn Date_valueOf(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    
    Ok(env.new_number(time))
}

// 15.9.5.9 Date.prototype.getTime ( )
pub fn Date_getTime(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    Date_valueOf(env, mode, args)
}

macro_rules! time_getter {
    ( $getter:ident , $utc_getter:ident , $function:ident ) => {
        pub fn $getter(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
            let time = try!(get_time_from_args(env, &args));
            
            let time = if time.is_nan() {
                f64::NAN
            } else {
                $function(get_local_time(time))
            };
            
            Ok(env.new_number(time))
        }
        
        
        pub fn $utc_getter(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
            let time = try!(get_time_from_args(env, &args));
            
            let time = if time.is_nan() {
                f64::NAN
            } else {
                $function(time)
            };
            
            Ok(env.new_number(time))
        }
    }
}

// 15.9.5.10 Date.prototype.getFullYear ( )
// 15.9.5.11 Date.prototype.getUTCFullYear ( )
time_getter!(Date_getFullYear, Date_getUTCFullYear, get_year_from_time);

// 15.9.5.12 Date.prototype.getMonth ( )
// 15.9.5.13 Date.prototype.getUTCMonth ( )
time_getter!(Date_getMonth, Date_getUTCMonth, get_month_from_time);

// 15.9.5.14 Date.prototype.getDate ( )
// 15.9.5.15 Date.prototype.getUTCDate ( )
time_getter!(Date_getDate, Date_getUTCDate, get_date_from_time);

// 15.9.5.16 Date.prototype.getDay ( )
// 15.9.5.17 Date.prototype.getUTCDay ( )
time_getter!(Date_getDay, Date_getUTCDay, get_week_day);

// 15.9.5.18 Date.prototype.getHours ( )
// 15.9.5.19 Date.prototype.getUTCHours ( )
time_getter!(Date_getHours, Date_getUTCHours, get_hour_from_time);

// 15.9.5.20 Date.prototype.getMinutes ( )
// 15.9.5.21 Date.prototype.getUTCMinutes ( )
time_getter!(Date_getMinutes, Date_getUTCMinutes, get_min_from_time);

// 15.9.5.22 Date.prototype.getSeconds ( )
// 15.9.5.23 Date.prototype.getUTCSeconds ( )
time_getter!(Date_getSeconds, Date_getUTCSeconds, get_sec_from_time);

// 15.9.5.24 Date.prototype.getMilliseconds ( )
// 15.9.5.25 Date.prototype.getUTCMilliseconds ( )
time_getter!(Date_getMilliseconds, Date_getUTCMilliseconds, get_ms_from_time);

// 15.9.5.26 Date.prototype.getTimezoneOffset ( )
pub fn Date_getTimezoneOffset(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    
    let time = if time.is_nan() {
        f64::NAN
    } else {
        (time - get_local_time(time)) / MS_PER_MINUTE
    };
    
    Ok(env.new_number(time))
}

// 15.9.5.27 Date.prototype.setTime (time)
pub fn Date_setTime(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let this = args.this(env);
    
    if this.ty() == JsType::Object {
        let mut this = this.unwrap_object(env);
        if let Some(prototype) = this.prototype(env) {
            if
                prototype.ty() == JsType::Object &&
                prototype.unwrap_object(env).as_ptr() == env.handle(JsHandle::Date).as_ptr()
            {
                let time = time_clip(try!(args.arg(env, 0).to_number(env)));
                let time = env.new_number(time);
                
                this.set_value(time);
                
                return Ok(time);
            }
        }
    }
    
    Err(JsError::new_type(env, ::errors::TYPE_NOT_DATE))
}

// 15.9.5.28 Date.prototype.setMilliseconds (ms)
pub fn Date_setMilliseconds(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    let time = get_local_time(time);
    
    let time = make_time(
        get_hour_from_time(time),
        get_min_from_time(time),
        get_sec_from_time(time),
        try!(args.arg(env, 0).to_number(env))
    );
    
    let time = time_clip(get_utc(make_date(get_day(time), time)));
    let time = env.new_number(time);
    
    args.this(env).unwrap_object(env).set_value(time);
    
    Ok(time)
}

// 15.9.5.29 Date.prototype.setUTCMilliseconds (ms)
pub fn Date_setUTCMilliseconds(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    
    let time = make_time(
        get_hour_from_time(time),
        get_min_from_time(time),
        get_sec_from_time(time),
        try!(args.arg(env, 0).to_number(env))
    );
    
    let time = time_clip(make_date(get_day(time), time));
    let time = env.new_number(time);
    
    args.this(env).unwrap_object(env).set_value(time);
    
    Ok(time)
}

// 15.9.5.30 Date.prototype.setSeconds (sec [, ms ] )
pub fn Date_setSeconds(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    let time = get_local_time(time);
    
    let sec = try!(args.arg(env, 0).to_number(env));
    let ms = try!(args.map_or_else(env, 1, |_| Ok(get_ms_from_time(time)), |env, p| p.to_number(env)));
    
    let time = make_time(
        get_hour_from_time(time),
        get_min_from_time(time),
        sec,
        ms
    );
    
    let time = time_clip(get_utc(make_date(get_day(time), time)));
    let time = env.new_number(time);
    
    args.this(env).unwrap_object(env).set_value(time);
    
    Ok(time)
}

// 15.9.5.31 Date.prototype.setUTCSeconds (sec [, ms ] )
pub fn Date_setUTCSeconds(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    
    let sec = try!(args.arg(env, 0).to_number(env));
    let ms = try!(args.map_or_else(env, 1, |_| Ok(get_ms_from_time(time)), |env, p| p.to_number(env)));
    
    let time = make_time(
        get_hour_from_time(time),
        get_min_from_time(time),
        sec,
        ms
    );
    
    let time = time_clip(make_date(get_day(time), time));
    let time = env.new_number(time);
    
    args.this(env).unwrap_object(env).set_value(time);
    
    Ok(time)
}

// 15.9.5.32 Date.prototype.setMinutes (min [, sec [, ms ] ] )
pub fn Date_setMinutes(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    let time = get_local_time(time);
    
    let min = try!(args.arg(env, 0).to_number(env));
    let sec = try!(args.map_or_else(env, 1, |_| Ok(get_sec_from_time(time)), |env, p| p.to_number(env)));
    let ms = try!(args.map_or_else(env, 2, |_| Ok(get_ms_from_time(time)), |env, p| p.to_number(env)));
    
    let time = make_time(
        get_hour_from_time(time),
        min,
        sec,
        ms
    );
    
    let time = time_clip(get_utc(make_date(get_day(time), time)));
    let time = env.new_number(time);
    
    args.this(env).unwrap_object(env).set_value(time);
    
    Ok(time)
}

// 15.9.5.33 Date.prototype.setUTCMinutes (min [, sec [, ms ] ] )
pub fn Date_setUTCMinutes(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    
    let min = try!(args.arg(env, 0).to_number(env));
    let sec = try!(args.map_or_else(env, 1, |_| Ok(get_sec_from_time(time)), |env, p| p.to_number(env)));
    let ms = try!(args.map_or_else(env, 2, |_| Ok(get_ms_from_time(time)), |env, p| p.to_number(env)));
    
    let time = make_time(
        get_hour_from_time(time),
        min,
        sec,
        ms
    );
    
    let time = time_clip(make_date(get_day(time), time));
    let time = env.new_number(time);
    
    args.this(env).unwrap_object(env).set_value(time);
    
    Ok(time)
}

// 15.9.5.34 Date.prototype.setHours (hour [, min [, sec [, ms ] ] ] )
pub fn Date_setHours(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    let time = get_local_time(time);
    
    let hour = try!(args.arg(env, 0).to_number(env));
    let min = try!(args.map_or_else(env, 1, |_| Ok(get_min_from_time(time)), |env, p| p.to_number(env)));
    let sec = try!(args.map_or_else(env, 2, |_| Ok(get_sec_from_time(time)), |env, p| p.to_number(env)));
    let ms = try!(args.map_or_else(env, 3, |_| Ok(get_ms_from_time(time)), |env, p| p.to_number(env)));
    
    let time = make_time(
        hour,
        min,
        sec,
        ms
    );
    
    let time = time_clip(get_utc(make_date(get_day(time), time)));
    let time = env.new_number(time);
    
    args.this(env).unwrap_object(env).set_value(time);
    
    Ok(time)
}

// 15.9.5.35 Date.prototype.setUTCHours (hour [, min [, sec [, ms ] ] ] )
pub fn Date_setUTCHours(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    
    let hour = try!(args.arg(env, 0).to_number(env));
    let min = try!(args.map_or_else(env, 1, |_| Ok(get_min_from_time(time)), |env, p| p.to_number(env)));
    let sec = try!(args.map_or_else(env, 2, |_| Ok(get_sec_from_time(time)), |env, p| p.to_number(env)));
    let ms = try!(args.map_or_else(env, 3, |_| Ok(get_ms_from_time(time)), |env, p| p.to_number(env)));
    
    let time = make_time(
        hour,
        min,
        sec,
        ms
    );
    
    let time = time_clip(make_date(get_day(time), time));
    let time = env.new_number(time);
    
    args.this(env).unwrap_object(env).set_value(time);
    
    Ok(time)
}

// 15.9.5.36 Date.prototype.setDate (date)
pub fn Date_setDate(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    let time = get_local_time(time);
    
    let date = try!(args.arg(env, 0).to_number(env));
    
    let new_date = make_date(
        make_day(get_year_from_time(time), get_month_from_time(time), date),
        get_time_within_day(time)
    );
    
    let time = time_clip(get_utc(new_date));
    let time = env.new_number(time);
    
    args.this(env).unwrap_object(env).set_value(time);
    
    Ok(time)
}

// 15.9.5.37 Date.prototype.setUTCDate (date)
pub fn Date_setUTCDate(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    
    let date = try!(args.arg(env, 0).to_number(env));
    
    let new_date = make_date(
        make_day(get_year_from_time(time), get_month_from_time(time), date),
        get_time_within_day(time)
    );
    
    let time = time_clip(new_date);
    let time = env.new_number(time);
    
    args.this(env).unwrap_object(env).set_value(time);
    
    Ok(time)
}

// 15.9.5.38 Date.prototype.setMonth (month [, date ] )
pub fn Date_setMonth(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    let time = get_local_time(time);
    
    let month = try!(args.arg(env, 0).to_number(env));
    let date = try!(args.map_or_else(env, 1, |_| Ok(get_date_from_time(time)), |env, p| p.to_number(env)));
    
    let new_date = make_date(
        make_day(get_year_from_time(time), month, date),
        get_time_within_day(time)
    );
    
    let time = time_clip(get_utc(new_date));
    let time = env.new_number(time);
    
    args.this(env).unwrap_object(env).set_value(time);
    
    Ok(time)
}

// 15.9.5.39 Date.prototype.setUTCMonth (month [, date ] )
pub fn Date_setUTCMonth(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    
    let month = try!(args.arg(env, 0).to_number(env));
    let date = try!(args.map_or_else(env, 1, |_| Ok(get_date_from_time(time)), |env, p| p.to_number(env)));
    
    let new_date = make_date(
        make_day(get_year_from_time(time), month, date),
        get_time_within_day(time)
    );
    
    let time = time_clip(new_date);
    let time = env.new_number(time);
    
    args.this(env).unwrap_object(env).set_value(time);
    
    Ok(time)
}

// 15.9.5.40 Date.prototype.setFullYear (year [, month [, date ] ] )
pub fn Date_setFullYear(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    let time = get_local_time(time);
    
    let year = try!(args.arg(env, 0).to_number(env));
    let month = try!(args.map_or_else(env, 1, |_| Ok(get_month_from_time(time)), |env, p| p.to_number(env)));
    let date = try!(args.map_or_else(env, 2, |_| Ok(get_date_from_time(time)), |env, p| p.to_number(env)));
    
    let new_date = make_date(
        make_day(year, month, date),
        get_time_within_day(time)
    );
    
    let time = time_clip(get_utc(new_date));
    let time = env.new_number(time);
    
    args.this(env).unwrap_object(env).set_value(time);
    
    Ok(time)
}

// 15.9.5.41 Date.prototype.setUTCFullYear (year [, month [, date ] ] )
pub fn Date_setUTCFullYear(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    
    let year = try!(args.arg(env, 0).to_number(env));
    let month = try!(args.map_or_else(env, 1, |_| Ok(get_month_from_time(time)), |env, p| p.to_number(env)));
    let date = try!(args.map_or_else(env, 2, |_| Ok(get_date_from_time(time)), |env, p| p.to_number(env)));
    
    let new_date = make_date(
        make_day(year, month, date),
        get_time_within_day(time)
    );
    
    let time = time_clip(new_date);
    let time = env.new_number(time);
    
    args.this(env).unwrap_object(env).set_value(time);
    
    Ok(time)
}

// 15.9.5.42 Date.prototype.toUTCString ( )
pub fn Date_toUTCString(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    Date_toString(env, mode, args)
}

// 15.9.5.43 Date.prototype.toISOString ( )
pub fn Date_toISOString(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    if time.is_nan() {
        Err(JsError::new_range(env))
    } else {
        Date_toString(env, mode, args)
    }
}

// 15.9.5.44 Date.prototype.toJSON ( key )
pub fn Date_toJSON(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let object = try!(args.this(env).to_object(env));
    let time = try!(object.to_primitive(env, JsPreferredType::Number));
    
    if time.ty() == JsType::Number && !time.unwrap_number().is_finite() {
        Ok(env.new_null())
    } else {
        let to_iso = try!(object.get(env, name::TO_ISO));
        
        if !to_iso.is_callable(env) {
            Err(JsError::new_type(env, ::errors::TYPE_NOT_CALLABLE))
        } else {
            to_iso.call(env, object, Vec::new(), false)
        }
    }
}

// B.2.4 Date.prototype.getYear ( )
pub fn Date_getYear(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    
    let time = if time.is_nan() {
        f64::NAN
    } else {
        get_year_from_time(get_local_time(time)) - 1900.0
    };
    
    Ok(env.new_number(time))
}

// B.2.5 Date.prototype.setYear (year)
pub fn Date_setYear(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    let time = try!(get_time_from_args(env, &args));
    let time = get_local_time(time);
    
    let year = try!(args.arg(env, 0).to_number(env));
    let time = if year.is_nan() {
        f64::NAN
    } else {
        let year = year.trunc();
        let year = if year >= 0.0 && year <= 99.0 { year + 1900.0 } else { year };
        
        make_date(
            make_day(year, get_month_from_time(time), get_date_from_time(time)),
            get_time_within_day(time)
        )
    };
    
    let time = time_clip(get_utc(time));
    let time = env.new_number(time);
    
    args.this(env).unwrap_object(env).set_value(time);
    
    Ok(time)
}

// B.2.6 Date.prototype.toGMTString ( )
pub fn Date_toGMTString(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<gc::Local<JsValue>> {
    Date_toUTCString(env, mode, args)
}
