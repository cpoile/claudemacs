# Claudemacs TODOs

## Bugs to Fix

### UTF-8 Encoding for eat terminals
- **Issue**: eat terminal buffers (like `*eat-test*`) don't default to UTF-8 encoding
- **Impact**: May cause issues with non-ASCII characters in terminal output
- **Fix**: Need to set `buffer-file-coding-system` or similar when creating eat buffers
- **Location**: Likely in `claudemacs--start` or eat terminal creation code

## Potential Improvements

### Shell Integration Detection
- Add helper function to check if eat shell integration is active
- Could warn users if `exec-in-terminal` is used without shell integration

### Better Error Messages
- Provide more helpful errors when commands timeout
- Suggest enabling shell integration if prompt detection fails frequently
