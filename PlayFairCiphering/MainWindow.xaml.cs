using MaterialDesignThemes.Wpf;
using Microsoft.Win32;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace PlayFairCiphering
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        [DllImport("C:\\Users\\hp\\source\\repos\\PlayFairCiphering\\Project.dll")]
        public static extern void Encryption([In]char[] x, int count, [In, Out]char[] y);
        [DllImport("C:\\Users\\hp\\source\\repos\\PlayFairCiphering\\Project.dll")]
        public static extern void Decryption([In]char[] x, int count, [In, Out]char[] y);
        [DllImport("C:\\Users\\hp\\source\\repos\\PlayFairCiphering\\Project.dll")]
        public static extern void EncryptBouns([In]char[] x, int count, [In, Out]char[] y);
        [DllImport("C:\\Users\\hp\\source\\repos\\PlayFairCiphering\\Project.dll")]
        public static extern void DecryptBouns([In]char[] x, int count, [In, Out]char[] y);
        private static bool encrypt_pfair;
        private static bool decrypt_pfair;
        private static bool encrypt_vig;
        private static bool decrypt_vig;
        public MainWindow()
        {
            InitializeComponent();
        }

        private void MenuButtonMainLeft_Click(object sender, RoutedEventArgs e)
        {

            
        }

        private void Close_icon(object sender, RoutedEventArgs e)
        {
            Close();
        }

        private void MinMax(object sender, RoutedEventArgs e)
        {
            Thickness marginThickness = Menu.Margin;
            if (WindowState != WindowState.Maximized)
            {
                this.WindowState = WindowState.Maximized;
                maxOrMin.Kind = PackIconKind.SquareInc;

                if (marginThickness.Left == 0)
                {
                    MenuGrid.Width = 50;
                    DisplayArea.Width = 1330;
                    plntContent.Width = 1373;
                }
                else
                {
                    Menu.Margin = new Thickness(195, 0, 0, 0);
                    MenuGrid.Width = 250;
                    DisplayArea.Width = 1130;
                    plntContent.Width = 1073;
                }

            }
            else
            {
                this.WindowState = WindowState.Normal;
                maxOrMin.Kind = PackIconKind.SquareOutline;
                if (marginThickness.Left == 0)
                {
                    Menu.Margin = new Thickness(0, 0, 0, 0);
                    MenuGrid.Width = 50;
                    DisplayArea.Width = 1060;
                    plntContent.Width = 1003;
                }
                else
                {
                    Menu.Margin = new Thickness(145, 0, 0, 0);
                    MenuGrid.Width = 200;
                    DisplayArea.Width = 910;
                    plntContent.Width = 853;
                }


            }

        }

        private void MiniMiz(object sender, RoutedEventArgs e)
        {
            this.WindowState = WindowState.Minimized;
        }

     
       

        private void ListViewItem_MouseEnter(object sender, MouseEventArgs e)
        {

        }


        

        private void ListViewItem_Selected(object sender, RoutedEventArgs e)
        {
            this.path_file.Visibility = Visibility.Visible;
            this.Home_page.Visibility = Visibility.Collapsed;
            this.Theme_page.Visibility = Visibility.Collapsed;
            this.About_page.Visibility = Visibility.Collapsed;
            this.help.Visibility = Visibility.Collapsed;
            encrypt_pfair = true;
        }

        private void ListViewItem_Selected_1(object sender, RoutedEventArgs e)
        {
            this.path_file.Visibility = Visibility.Visible;
            this.Home_page.Visibility = Visibility.Collapsed;
            this.Theme_page.Visibility = Visibility.Collapsed;
            this.About_page.Visibility = Visibility.Collapsed;
            this.help.Visibility = Visibility.Collapsed;
            decrypt_pfair = true;
        }

        private void ListViewItem_Selected_2(object sender, RoutedEventArgs e)
        {
            this.path_file.Visibility = Visibility.Visible;
            this.Home_page.Visibility = Visibility.Collapsed;
            this.Theme_page.Visibility = Visibility.Collapsed;
            this.About_page.Visibility = Visibility.Collapsed;
            this.help.Visibility = Visibility.Collapsed;
            encrypt_vig = true;
        }

        private void ListViewItem_Selected_3(object sender, RoutedEventArgs e)
        {
            this.path_file.Visibility = Visibility.Visible;
            this.Home_page.Visibility = Visibility.Collapsed;
            this.Theme_page.Visibility = Visibility.Collapsed;
            this.About_page.Visibility = Visibility.Collapsed;
            this.help.Visibility = Visibility.Collapsed;
            decrypt_vig = true;
        }

    

        private void home_fun(object sender, RoutedEventArgs e)
        {
            this.Home_page.Visibility = Visibility.Visible;
            this.Theme_page.Visibility = Visibility.Collapsed;
            this.path_file.Visibility = Visibility.Collapsed;
            this.About_page.Visibility = Visibility.Collapsed;
            this.help.Visibility = Visibility.Collapsed;
        }

        private void theme_fun(object sender, RoutedEventArgs e)
        {
            this.Theme_page.Visibility = Visibility.Visible;
            this.Home_page.Visibility = Visibility.Collapsed;
            this.path_file.Visibility = Visibility.Collapsed;
            this.About_page.Visibility = Visibility.Collapsed;
            this.help.Visibility = Visibility.Collapsed;
        }

        private void help_fun(object sender, RoutedEventArgs e)
        {
            this.Theme_page.Visibility = Visibility.Collapsed;
            this.Home_page.Visibility = Visibility.Collapsed;
            this.About_page.Visibility = Visibility.Collapsed;
            this.help.Visibility = Visibility.Visible;
            this.path_file.Visibility = Visibility.Collapsed;
        }


        private static string pat_f;
        private void ListViewItem_Selected_4(object sender, RoutedEventArgs e)
        {
            MessageBox.Show("op");
            FileDialog fileDialog = new OpenFileDialog();
            fileDialog.DefaultExt = ".txt";
            fileDialog.ShowDialog();
            if (fileDialog.FileName != null)
            {
                this.path.Text = fileDialog.FileName;
                pat_f= fileDialog.FileName;
            }
            else
            {
                MessageBox.Show("no file select");
            }
        }
        
        private void ListViewItem_Selected_5(object sender, RoutedEventArgs e)
        {
           string outfilename="no file";
            try
            {
                if (encrypt_pfair && pat_f != null )
                {
                    
                     char[] c = pat_f.ToCharArray();
                    char[] out_f = new char[100];
                    Encryption(c, c.Length, out_f);
                     outfilename = new string(out_f);
                    MessageBox.Show("path of Message is :"+outfilename);
                }
                else if (decrypt_pfair && pat_f != null)
                {
                    char[] c = pat_f.ToCharArray();
                    char[] out_f = new char[100];
                    Decryption(c, c.Length, out_f);
                     outfilename = new string(out_f);
                    MessageBox.Show("path of Message is : " + outfilename);
                }
                else if (encrypt_vig && pat_f!=null)
                {
                    char[] c = pat_f.ToCharArray();
                    char[] out_f = new char[100];
                    EncryptBouns(c, c.Length, out_f);
                     outfilename = new string(out_f);
                    MessageBox.Show("path of Message is : " + outfilename);
                }
                else if (decrypt_vig &&  pat_f!=null)
                {
                    char[] c = pat_f.ToCharArray();
                    char[] out_f = new char[100];
                    DecryptBouns(c, c.Length, out_f);
                     outfilename = new string(out_f);
                    MessageBox.Show("path of Message is : " + outfilename);
                }

            }
            catch (DivideByZeroException e1)
            {
                MessageBox.Show(outfilename);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
        }

        private void ListViewItem_Selected_6(object sender, RoutedEventArgs e)
        {

            this.Left_side.Background = new SolidColorBrush(Colors.Black);
        }

        private void ListViewItem_Selected_7(object sender, RoutedEventArgs e)
        {
            this.Left_side.Background = new SolidColorBrush(Colors.CadetBlue);

        }

        private void ListViewItem_Selected_8(object sender, RoutedEventArgs e)
        {
            this.Left_side.Background = new SolidColorBrush(Colors.DarkCyan);

        }

        private void ListViewItem_Selected_9(object sender, RoutedEventArgs e)
        {
            this.Left_side.Background = new SolidColorBrush(Colors.DarkTurquoise);


        }

        private void ListViewItem_Selected_10(object sender, RoutedEventArgs e)
        {
            this.Left_side.Background = new SolidColorBrush(Colors.DarkSlateGray);

        }

        private void ListViewItem_Selected_11(object sender, RoutedEventArgs e)
        {
            this.Left_side.Background = new SolidColorBrush(Colors.BurlyWood);

        }

        private void ListViewItem_Selected_12(object sender, RoutedEventArgs e)
        {
            this.Left_side.Background = new SolidColorBrush(Colors.Lavender);

        }

        private void ListViewItem_Selected_13(object sender, RoutedEventArgs e)
        {
            this.Left_side.Background = new SolidColorBrush(Colors.Khaki);

        }

        private void about_fun(object sender, RoutedEventArgs e)
        {
            this.Theme_page.Visibility = Visibility.Collapsed;
            this.Home_page.Visibility = Visibility.Collapsed;
            this.About_page.Visibility = Visibility.Visible;
            this.help.Visibility = Visibility.Collapsed;
        }
    }
    }
