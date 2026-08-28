param(
    [string] $Title = 'Claudemacs',
    [string] $Message = 'Claudemacs is awaiting your input',
    [int] $TimeoutSeconds = 5,
    [switch] $Install,
    [string] $TargetPath
)

$ErrorActionPreference = 'Stop'
$appId = 'Claudemacs.Emacs'
$shortcutPath = Join-Path ([Environment]::GetFolderPath('StartMenu')) 'Programs\Claudemacs.lnk'
$pendingShortcutPath = Join-Path ([Environment]::GetFolderPath('StartMenu')) 'Programs\Claudemacs.installing.lnk'

function Set-ShortcutAppId([string] $Path, [string] $Id) {
    if (-not ('Claudemacs.ShortcutProperty' -as [type])) {
        Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;

namespace Claudemacs {
    [StructLayout(LayoutKind.Sequential, Pack = 4)]
    public struct PropertyKey {
        public Guid FormatId;
        public UInt32 PropertyId;
    }

    [StructLayout(LayoutKind.Explicit)]
    public struct PropVariant {
        [FieldOffset(0)] public ushort ValueType;
        [FieldOffset(8)] public IntPtr PointerValue;
    }

    [ComImport]
    [Guid("886D8EEB-8CF2-4446-8D02-CDBA1DBDCF99")]
    [InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    interface IPropertyStore {
        uint GetCount();
        void GetAt(uint propertyIndex, out PropertyKey key);
        void GetValue(ref PropertyKey key, out PropVariant value);
        void SetValue(ref PropertyKey key, ref PropVariant value);
        void Commit();
    }

    public static class ShortcutProperty {
        [DllImport("shell32.dll")]
        static extern void SHChangeNotify(
            uint eventId, uint flags, IntPtr item1, IntPtr item2);

        [DllImport("shell32.dll", CharSet = CharSet.Unicode, PreserveSig = true)]
        static extern int SHGetPropertyStoreFromParsingName(
            string path, IntPtr bindContext, uint flags, ref Guid interfaceId,
            [MarshalAs(UnmanagedType.Interface)] out IPropertyStore propertyStore);

        public static void SetAppId(string path, string appId) {
            Guid interfaceId = new Guid("886D8EEB-8CF2-4446-8D02-CDBA1DBDCF99");
            IPropertyStore store;
            int result = SHGetPropertyStoreFromParsingName(
                path, IntPtr.Zero, 2, ref interfaceId, out store);
            Marshal.ThrowExceptionForHR(result);

            var key = new PropertyKey {
                FormatId = new Guid("9F4C2855-9F79-4B39-A8D0-E1D42DE1D5F3"),
                PropertyId = 5
            };
            var value = new PropVariant {
                ValueType = 31,
                PointerValue = Marshal.StringToCoTaskMemUni(appId)
            };
            try {
                store.SetValue(ref key, ref value);
                store.Commit();
            } finally {
                Marshal.FreeCoTaskMem(value.PointerValue);
                Marshal.ReleaseComObject(store);
            }
            SHChangeNotify(0x08000000, 0, IntPtr.Zero, IntPtr.Zero);
        }
    }
}
'@
    }
    [Claudemacs.ShortcutProperty]::SetAppId($Path, $Id)
}

if ($Install) {
    if (-not $TargetPath) {
        throw 'TargetPath is required when installing the notification identity.'
    }
    $shell = New-Object -ComObject WScript.Shell
    $shortcut = $shell.CreateShortcut($pendingShortcutPath)
    # The shortcut registers the AppUserModelID only.  Toasts launch this
    # helper directly, so package moves cannot leave an executable stale link.
    $shortcut.TargetPath = $TargetPath
    $shortcut.Arguments = ''
    $shortcut.WorkingDirectory = Split-Path $TargetPath
    $shortcut.IconLocation = "$TargetPath,0"
    $shortcut.Description = 'Emacs with Claudemacs notifications'
    $shortcut.Save()
    Set-ShortcutAppId $pendingShortcutPath $appId
    Move-Item -LiteralPath $pendingShortcutPath -Destination $shortcutPath -Force
    $registrationPath = "HKCU:\Software\Classes\AppUserModelId\$appId"
    New-Item -Path $registrationPath -Force | Out-Null
    New-ItemProperty -Path $registrationPath -Name DisplayName -Value 'Claudemacs' -PropertyType ExpandString -Force | Out-Null
    New-ItemProperty -Path $registrationPath -Name IconUri -Value $TargetPath -PropertyType ExpandString -Force | Out-Null
    New-ItemProperty -Path $registrationPath -Name IconBackgroundColor -Value 'FF202020' -PropertyType String -Force | Out-Null
    New-ItemProperty -Path $registrationPath -Name ShowInSettings -Value 1 -PropertyType DWord -Force | Out-Null
    Write-Output $shortcutPath
    exit 0
}

if (-not (Test-Path $shortcutPath)) {
    throw 'Claudemacs notification identity is not installed.'
}

if (-not ('Claudemacs.ProcessIdentity' -as [type])) {
    Add-Type -TypeDefinition @'
using System.Runtime.InteropServices;

namespace Claudemacs {
    public static class ProcessIdentity {
        [DllImport("shell32.dll", CharSet = CharSet.Unicode)]
        public static extern int SetCurrentProcessExplicitAppUserModelID(string appId);
    }
}
'@
}
$identityResult = [Claudemacs.ProcessIdentity]::SetCurrentProcessExplicitAppUserModelID($appId)
if ($identityResult -ne 0) {
    [Runtime.InteropServices.Marshal]::ThrowExceptionForHR($identityResult)
}

[Windows.UI.Notifications.ToastNotificationManager, Windows.UI.Notifications, ContentType = WindowsRuntime] | Out-Null
[Windows.UI.Notifications.ToastNotification, Windows.UI.Notifications, ContentType = WindowsRuntime] | Out-Null

$xml = [Windows.UI.Notifications.ToastNotificationManager]::GetTemplateContent(
    [Windows.UI.Notifications.ToastTemplateType]::ToastText02)
$xml.DocumentElement.SetAttribute('duration', 'short')
$nodes = $xml.GetElementsByTagName('text')
$nodes.Item(0).AppendChild($xml.CreateTextNode($Title)) | Out-Null
$nodes.Item(1).AppendChild($xml.CreateTextNode($Message)) | Out-Null
$toast = [Windows.UI.Notifications.ToastNotification]::new($xml)
$toast.ExpirationTime = [DateTimeOffset]::Now.AddSeconds($TimeoutSeconds)
$notifier = [Windows.UI.Notifications.ToastNotificationManager]::CreateToastNotifier($appId)
$notifier.Show($toast)

# ExpirationTime only prevents stale queued delivery; it does not close a toast
# that Windows is already displaying.  Keep this hidden helper alive long enough
# to dismiss the specific notification explicitly.
if ($TimeoutSeconds -gt 0) {
    Start-Sleep -Seconds $TimeoutSeconds
    $notifier.Hide($toast)
}
